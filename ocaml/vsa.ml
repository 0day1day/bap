(** Value-Set Analysis / Value-Set Arithmetic

    See Gogul Balakrishnan's thesis at
    http://pages.cs.wisc.edu/~bgogul/Research/Thesis/thesis.html

    TODO:
    * Alternate memstore implementation
    * Add a real interface; automatically call simplify_cond
    * Big int support
    * Idea: Use copy propagation information to maintain equivalence
      classes, and use intersection over equivalence class members at
      edge transfer
    * Partial/overlapping memory
    * Special case memory writes of Top: since we will be removing
      entries, we do not have to iterate through all addresses
    * Unified interface to Arithmetic for singleton values
    * Strided-interval aware applicative data type:
      It would store values as strided intervals, rather than
      individual points.
*)

module VM = Var.VarMap

open Big_int_convenience
open Big_int_Z
open Util
open Type
open Ast

module D = Debug.Make(struct let name = "Vsa" and default=`NoDebug end)
open D
module DV = Debug.Make(struct let name = "VsaVerbose" and default=`NoDebug end)

(* Treat unsigned comparisons the same as signed: should be okay as
   long as overflow does not occur. Should be false for soundness. *)
let signedness_hack = ref true

(* Set memory to top once it surpasses this number of entries *)
let mem_max = ref (Some(1 lsl 16))

exception Unimplemented of string

module I = Int64
(* some operators to make this more readable *)
let (&%) = I.logand
let (|%) = I.logor
let (^%) = I.logxor
let (+%) = I.add
let (-%) = I.sub
let ( *%) = I.mul
let (/%) = I.div
let bnot = I.lognot

let rec uint64_gcd x y =
  if y = 0L then x
  else uint64_gcd y (int64_urem x y)

let uint64_lcm x y =
  let m = x *% y in
  m /% (uint64_gcd x y)

let bits_of_width = Typecheck.bits_of_width

let sp = Disasm_i386.esp

(* FIXME *)
let addr_bits = 32

(** Strided Intervals *)
module SI =
struct
(* FIXME: some of these functions can return a stride of 1 for a singleton *)

  (** number of bits * unsigned stride * signed lower bound * signed upper bound *)
  type t = int * int64 * int64 * int64

  let is_empty (k,s,lb,ub) =
    s = (-1L) && lb = 1L && ub = 0L

  let to_string ((k,s,lb,ub) as si) =
    if is_empty si then "[]"
    else if not (debug ()) then Printf.sprintf "%Lu[%Ld,%Ld]" s lb ub
    else Printf.sprintf "(%d)%Lu[%Ld,%Ld]" k s lb ub

  let size (_,s,lb,ub) =
    (ub -% lb) /% s

  let highbit k =
    if k = 1 then 1L else I.shift_left 1L (k-1)

  (* Cast i as signed number *)
  let extend k i =
    if k <> 64 then
      let k' = 64-k in I.shift_right(I.shift_left i k') k'
    else i

  (* Set the irrelevant upper bits of i to 0 *)
  let trunc k i =
    if k <> 64 then
      let k' = 64-k in I.shift_right_logical(I.shift_left i k') k'
    else i

  let maxi k = highbit k -% 1L
  let mini k = extend k (highbit k)
  let top k = (k, 1L, mini k, maxi k)
  let empty k = (k, (-1L), 1L, 0L)

  let rec upper k i s =
    if s >= 1L then (
    let offset = int64_urem i s in
    let max = maxi k in
    let maxoffset = int64_urem max s in
    let o = if maxoffset >= offset then
        max -% (maxoffset -% offset)
      else
        max -% ((maxoffset +% s) -% offset)
    in
    if debug ()
    then (assert (o <= maxi k && o > maxi k -% s);
          assert (int64_urem o s = int64_urem i s));
    o) else maxi k

  let rec lower k i s =
    if s >= 1L then (
    let offset = int64_urem i s in
    let min = mini k in
    let minoffset = int64_urem min s in
    let o = if offset >= minoffset then
        min +% (offset -% minoffset)
      else
        min +% ((offset +% s) -% minoffset)
    in
    if debug ()
    then (assert (o >= mini k && o < mini k +% s);
          assert (int64_urem o s = int64_urem i s));
    o) else mini k

  let remove_lower_bound (k,s,a,b) =
    (k,s,lower k b s,b)
  let remove_upper_bound (k,s,a,b) =
    (k,s,a,upper k a s)

  let single k x = (k,0L,x,x)
  let of_bap_int i t = single (bits_of_width t) (extend (bits_of_width t) i)

  let above k x = (k, 1L, x +% 1L, maxi k)
  let below k x = (k, 1L, mini k, x -% 1L)

  (* These are hacks *)
  let above_unsigned k x = (k, 1L, x +% 1L, maxi k)
  let below_unsigned k x = (k, 1L, 0L, x -% 1L)

  let aboveeq k x = (k, 1L, x, maxi k)
  let beloweq k x = (k, 1L, mini k, x)

  (* These are hacks *)
  let aboveeq_unsigned k x = (k, 1L, x, maxi k)
  let beloweq_unsigned k x = (k, 1L, 0L, x)

  let zero k = single k 0L
  let one k = single k 1L
  let minus_one k = single k (-1L)

  (* XXX: Remove k argument *)
  let is_reduced k ((k',s,lb,ub) as si) =
    if k > 64 then raise (Unimplemented (Printf.sprintf "Register type of %d bits is too large (must be <= 64)" k));
    assert(k=k');
    assert(k>0 && k<=64);
    (lb >= mini k && ub <= maxi k) &&
      is_empty si ||
      (if s = 0L then lb = ub
       else lb < ub && let r1 = I.rem lb s and r2 = I.rem ub s in r1 = r2 || r2 -% r1 = s)

  let check_reduced k si =
    if not(is_reduced k si)
    then failwith(string_of_int k^"-bit Strided Interval "^to_string si^" not in reduced form")

  let check_reduced2 k si1 si2 =
    check_reduced k si1; check_reduced k si2

  (* XXX: Remove k argument *)
  let renorm k ((k',a,b,c) as si) =
    assert(k=k');
    let si' = if b = c then (k,0L,b,b) else si in
    let si' = if a < 0L || c < b then empty k else si' in
    check_reduced k si';
    si'

  let renormtri f k x y z = renorm k (f k x y z)
  let renormbin f k x y = renorm k (f k x y)
  let renormun f k x = renorm k (f k x)

  (* For union/intersection which don't use k argument *)
  let renormbin' f ((k,_,_,_) as x) y = renorm k (f x y)

  (* XXX: Remove k *)
  (** Addition of strided intervals *)
  let add ?(allow_overflow=true) k ((k',s1,lb1,ub1) as a) ((k'',s2,lb2,ub2) as b) =
    assert (k=k' && k=k'');
    check_reduced2 k a b;
    let lb' = lb1 +% lb2
    and ub' = ub1 +% ub2 in
    (* Overflow cases 2 and 4; see dissertation *)
    let lbunderflow = lb' < mini k
    and uboverflow = ub' > maxi k
    and s = uint64_gcd s1 s2
    in
    let overflow = lbunderflow || uboverflow in
    match overflow with
    | true when allow_overflow ->
      top k
    | _ ->
      let lb' = extend k lb' in
      let ub' = extend k ub' in
      let lb'' = if lbunderflow then lower k lb' s else lb'
      and ub'' = if uboverflow then upper k ub' s else ub'
      in
      (k, s, lb'', ub'')

  let add ?allow_overflow k x y = renormbin (add ?allow_overflow) k x y

  (* XXX: Remove k *)
  (** Negation of a strided interval *)
  let neg k ((k',s,lb,ub) as si) =
    assert(k=k');
    check_reduced k si;
    if lb <> extend k (highbit k) then
      (k, s, I.neg ub, I.neg lb)
    else if lb = ub then 
      single k (mini k)
    else
      top k
  let neg = renormun neg

  (** Subtractionf of strided intervals *)
  let sub k a b =
    add k a (neg k b)
  let sub = renormbin sub

  let minor k a b c d =
    let rec loop m =
      let cont() = loop (I.shift_right_logical m 1) in
        if m = 0L then a |% c
        else if bnot a &% c &% m <> 0L then
          let temp = (a |% m ) &% I.neg m in
            if int64_ucompare temp b <= 0 then
              temp |% c
            else cont()
        else if a &% bnot c &% m  <> 0L then
          let temp = (c +% m) &% I.neg m in
            if int64_ucompare temp d <= 0 then
              temp  |% a
            else cont()
        else
          cont()
    in
      loop (highbit k)

  let maxor k a b c d =
    let rec loop m =
      let cont() = loop (I.shift_right_logical m 1) in
        if m = 0L then b |% d
        else if b &% d &% m <> 0L then
          let temp1 = (b -% m) |% (m -% 1L) in
          let temp2 = (d -% m) |% (m -% 1L) in
            if int64_ucompare temp1 a >= 0 then
              temp1 |% d
            else if int64_ucompare temp2 c >= 0 then
              temp2 |% b
            else cont()
        else
          cont()
    in
      loop (highbit k)

  let ntz x =
    let y = I.neg x &% (x -% 1L) in
    let rec bits n y =
      if y = 0L then n else bits (n+1) (I.shift_right y 1)
    in
      bits 0 y


  (** Bitwise OR *)
  let logor k ((k',s1,lb1,ub1) as a) ((k'',s2,lb2,ub2) as b) =
    assert (k=k' && k=k'');
    check_reduced2 k a b;
    let t = min (ntz s1) (ntz s2) in
    let s' = I.shift_left 1L t in
    let lowbits = (lb1 |% lb2) &% (s' -% 1L) in
    let (lb', ub') = match (lb1 < 0L, ub1 < 0L, lb2 < 0L, ub2 < 0L) with
      | (true, true, true, true)
      | (true, true, false, false)
      | (false, false, true, true)
      | (false, false, false, false) ->
          (minor k lb1 ub1 lb2 ub2, maxor k lb1 ub1 lb2 ub2)
      | (true, true, true, false) ->
          (lb1, -1L)
      | (true, false, true, true) ->
          (lb2, -1L)
      | (true, false, true, false) ->
          (min lb1 lb2, maxor k 0L ub1 0L ub2)
      | (true, false, false, false) ->
          (minor k lb1 (-1L) lb2 ub2, maxor k 0L ub1 lb2 ub2)
      | (false, false, true, false) ->
          (minor k lb1 ub1 lb2 (-1L), maxor k lb1 ub1 lb2 ub2)
      | _ -> failwith "Impossible: check_reduced prevents this"
    in
    let highmask = bnot(s' -% 1L) in
      (k, s', (lb' &% highmask) |% lowbits, (ub' &% highmask) |% lowbits)
  let logor = renormbin logor

  (* XXX: Get rid of _k *)
  (** Bitwise NOT *)
  let lognot (_k:int) (k,s,l,u) =
    assert (_k = k);
    (k, s, bnot u, bnot l)
  let lognot = renormun lognot


  (** Bitwise AND *)
  let logand k x y =
    lognot k (logor k (lognot k x) (lognot k y))
  let logand = renormbin logand


  (** Bitwise XOR *)
  let logxor k x y =
    let n = lognot k
    and o = logor k in
    o (n(o (n x) y)) (n(o x (n y)))
  let logxor = renormbin logxor

  (** FIXME: Signed or unsigned modulus? *)
  let modulus k (k',s1,a,b) (k'',s2,c,d) =
    assert(k=k' && k=k'');
    if b = 0L then single k 0L
    else
      (k, 1L, 0L, int64_umin b d)
  let modulus = renormbin modulus

(* XXX: Get rid of k *)
(* shifting by more than k or by negative values
 * will be the same as shifting by k. *)
  let toshifts k =
    let f x = if x > Int64.of_int k || x < 0L then k else Int64.to_int x in
      function
        | (k',0L,x,y) ->
          assert(x=y);
          assert(k=k');
          let s = f x in
          (s,s)
        | (k',_s,x,y) ->
          assert(k=k');
          if x < 0L then
            if y >= 0L then
              (* FIXME: using stride information could be useful here *)
              (0, k)
            else (k,k)
          else (* x >= 0L *)
            (f x, f y)

  (* Get rid of k *)
  let mk_shift dir shifter k ((k',s1,a,b) as x) ((k'',_,_,_) as y) =
    assert(k=k' && k=k'');
    check_reduced2 k x y;
    (* Get the lower and upper bound for y, as shift amounts.  Shifts
       amounts are always in [0,k]. *)

    let (z1,z2) = toshifts k y
    (* Set the upper bits of a and b to 0 *)
    and aa = trunc k a
    and bb = trunc k b
    (* Shift and cast as signed number *)
    and shift n z = extend k (shifter n z) in

    (* Shift the lower bound by all possible shift amounts, and the
       upper bound by all possible shift amounts.  The min/max of the
       resulting values is the min/max bound of the shift result. *)

    let open BatPervasives in
    let mins = List.map (shift aa) (BatList.of_enum (z1--z2))
    and maxs = List.map (shift bb) (BatList.of_enum (z1--z2)) in
    let shifts = mins@maxs in

    let l = BatList.reduce min shifts in
    let u = BatList.reduce max shifts in

    let sign x = x >= 0L in
    let simpleshift =
    (* We have a simple shift if all shifts never change the sign of
       the value *)
      let minok = List.for_all (fun x -> sign x = sign aa) mins in
      let maxok = List.for_all (fun x -> sign x = sign bb) maxs in
      minok && maxok
    in

    let s' = match simpleshift, dir with
      | true, `Rightshift -> int64_umax (Int64.shift_right_logical s1 z2) 1L
      | true, `Leftshift -> int64_umax (Int64.shift_left s1 z2) 1L
      | false, _ -> 1L
    in
    renorm k (k,s',l,u)

  (** Logical right-shift *)
  let rshift = mk_shift `Rightshift Int64.shift_right_logical

  (** Arithmetic right-shift *)
  let arshift = mk_shift `Rightshift Int64.shift_right

  (** Left shift *)
  let lshift = mk_shift `Leftshift Int64.shift_left

  let cast_low tok ((k,s,a,b) as v) =
    assert (tok <= k);
    if tok = k then v
    else (
      let fits x = x = extend tok x in
      (* If a and b are in the lowest tok bits, just keep those! *)
      if fits s && fits a && fits b then
        (tok, s, a, b)
      else
        (* XXX: We can probably do better here *)
        top tok
    )
  let cast_low = renormun cast_low

  let cast_high tok ((k,s,a,b) as v) =
    assert (tok <= k);
    if tok = k then v
    else (
      (* Shift right, then cast low *)
      let v = rshift k v (single k (Int64.of_int(k - k))) in
      cast_low tok v)
  let cast_high = renormun cast_high

  let cast_signed tok ((k,s,a,b) as _v) =
    assert (tok >= k);
    (* Signed extension preserves signed values, so this is super
       easy! *)
    (tok,s,a,b)
  let cast_signed = renormun cast_signed

  let cast_unsigned tok ((k,s,a,b) as _v) =
    assert (tok >= k);
    (* Unsigned casting of signed numbers is bizarre.  For positive
       numbers, there is no problem, since sign-extension is the same as
       zero-extension for positive numbers.  For negative numbers,
       however, a negative number is transformed into a (large) positive
       number.  *)
    let c x =
      if x >= 0L then x
      else trunc k x in
    let a' = c a
    and b' = c b in
    (* Re-order if needed *)
    let a',b' = if a' <= b' then a',b' else b',a' in
    (tok,s,a',b')
  let cast_unsigned = renormun cast_unsigned

  let extract k h l ((k,_,_,_) as x) =
    let nb = (h-l)+1 in
    assert (h >= 0);
    assert (nb >= 0);
    let x = if l <> 0 then rshift k (single k (Int64.of_int l)) x else x in
    let x = if nb <> k then cast_low nb x else x in
    x
  let extract = renormtri extract

  let concat k (((k1,s1,l1,u1) as x) : t) (((k2,s2,l2,u2) as y) : t) =
    assert (k = k1 + k2);
    let x = cast_unsigned k x in
    let y = cast_unsigned k y in
    let x = lshift k x (single k (Int64.of_int k2)) in
    logor k x y
  let concat = renormbin concat

  (* construct these only once *)
  let yes = single 1 (-1L)
  and no = single 1 0L
  and maybe = (1, 1L, -1L, 0L)

  (* XXX: Remove k *)
  let eq k ((k',s1,a,b) as x) ((k'',s2,c,d) as y) =
    assert(k=k' && k=k'');
    check_reduced2 k x y;
    if a = b && a = c && a = d then
      yes
    else if b < c || d < a then
      no
    else
      let s' = uint64_gcd s1 s2 in
      let r1 = int64_urem a s'
      and r2 = int64_urem c s' in
        if r1 = r2 then
          maybe
        else
          no

  let union ((k,s1,a,b) as si1) ((k',s2,c,d) as si2) =
    if k <> k' then failwith "union: expected same bitwidth intervals";
    (* Empty sets *)
    if is_empty si1 then si2
    else if is_empty si2 then si1
    else
    let s' = uint64_gcd s1 s2 in
      if s' = 0L then
        if a = b && c = d then
          let u = max a c
          and l = min a c in
            (k, u -% l, l, u)
        else failwith "union: strided interval not in reduced form"
      else
        let r1 = I.rem a s' (* not right when s' is negative. *)
        and r2 = I.rem c s' in
        let u = max b d
        and l = min a c in
        if s' > 0L && r1 = r2 then
          (k, s', l, u)
        else
          let s'' = uint64_gcd (Int64.abs (r1 -% r2)) s' in
          (k, s'', l, u)
  let union = renormbin' union

  let intersection ((k,s1,a,b) as si1) ((k',s2,c,d) as si2) =
    if is_empty si1 || is_empty si2 then empty k
    else if k <> k' then failwith "intersection: expected same bitwidth intervals"
    else
    let l = max a c
    and u = min b d in
    if s1 = 0L && s2 = 0L then
      if a = c then (k,s1,a,b) else (empty k)
    else if s1 = 0L then
      if int64_urem (c -% a) s2 = 0L && a >= c && a <= d then (k,s1,a,b) else (empty k)
    else if s2 = 0L then
      if int64_urem (c -% a) s1 = 0L && c >= a && c <= b then (k',s2,c,d) else (empty k)
    else (
      let s' = uint64_lcm s1 s2 in
      if int64_urem a s' = 0L && int64_urem c s' = 0L then
        let l = l and u = u -% int64_urem u s' in
        if u >= l then (k, s', l, u -% int64_urem u s') else empty k
      else (k, 1L, l, u))
  let intersection = renormbin' intersection

  let widen ((k,s1,a,b) as si1) ((k',s2,c,d) as si2) =
    if is_empty si1 && not (is_empty si2) then top k
    else if is_empty si1 then si2
    else if is_empty si2 then si1
    else if k <> k' then failwith "widen: expected same bitwidth intervals"
    else
    (* dprintf "Widen: %s to %s" (to_string si1) (to_string si2); *)
    let s' = uint64_gcd s1 s2 in
    let l = if c < a then lower k a s' else a
    and u = if d > b then upper k b s' else b in
    if s' = 0L then
      if a = b && c = d then
        (k, u -% l, l, u)
      else failwith "widen: strided interval not in reduced form"
    else (k, s', l, u)
  let widen = renormbin' widen

  let rec fold f (k,s,a,b) init =
    if a = b then f a init
    else fold f (k, s, a+%s ,b) (f a init)

end (* module SI *)

(* Very simplified version of VSA, with no bounding *)
module SimpleVSA =
struct
  module DFP =
  struct
    module CFG = Cfg.AST
    module L =
    struct
      type t = SI.t VM.t
      let top = VM.empty
      let equal = VM.equal (=)
      let meet =
        VM.merge (fun k v1 v2 -> match v1, v2 with
        | Some v1, Some v2 -> Some (SI.union v1 v2)
        | Some v, None
        | None, Some v -> Some v
        | None, None -> None)
      let widen =
        VM.merge (fun k v1 v2 -> match v1, v2 with
        | Some v1, Some v2 -> Some (SI.widen v1 v2)
        | Some v, None
        | None, Some v -> Some v
        | None, None -> None)
    end
    module O = GraphDataflow.NOOPTIONS
    let s0 _ _ = CFG.G.V.create Cfg.BB_Entry
    let init _ g = L.top
    let dir _ = GraphDataflow.Forward

    let binop_to_si_function = function
      | PLUS -> SI.add ~allow_overflow:true
      | MINUS -> SI.sub
      | AND -> SI.logand
      | OR -> SI.logor
      | XOR -> SI.logxor
      | MOD -> SI.modulus
      | RSHIFT -> SI.rshift
      | ARSHIFT -> SI.arshift
      | LSHIFT -> SI.lshift
      | EQ -> SI.eq
      | NEQ -> fun k x y -> SI.lognot 1 (SI.eq k x y)
      | TIMES
      | DIVIDE
      | SDIVIDE
      | SMOD
      | LT
      | LE
      | SLT
      | SLE
          -> raise (Unimplemented "unimplemented binop")

    let unop_to_si_function = function
      | NEG -> SI.neg
      | NOT -> SI.lognot

    let cast_to_si_function = function
      | CAST_UNSIGNED -> SI.cast_unsigned
      | CAST_SIGNED -> SI.cast_signed
      | CAST_LOW -> SI.cast_low
      | CAST_HIGH -> SI.cast_high

    let bits_of_exp e = bits_of_width (Typecheck.infer_ast ~check:false e)

    let rec stmt_transfer_function _ _ _ s l =
      match s with
        | Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
        | Assert _ | Assume _ | Jmp _ | CJmp _ | Label _ | Comment _
        | Halt _ ->
            l
        | Special _ -> L.top
        | Move(v, e, _) ->
            try
              let top v = SI.top (bits_of_width(Var.typ v)) in
              let find v = VM.find v l in
              let do_find v =  try find v with Not_found -> top v in
              let rec exp2si e =
                try (match e with
                | Int(i,t) -> SI.of_bap_int (int64_of_big_int i) t
                | Lab _ -> raise(Unimplemented "No SI for labels (should be a constant)")
                | Var v -> do_find v
                | BinOp(op, x, y) ->
                  let f = binop_to_si_function op in
                  let k = bits_of_exp x in
                  let r = f k (exp2si x) (exp2si y) in
                  SI.check_reduced k r;
                  r
                | UnOp(op, x) ->
                  let f = unop_to_si_function op in
                  let k = bits_of_exp x in
                  let r = f k (exp2si x) in
                  SI.check_reduced k r;
                  r
                (* | Phi(x::xs) -> *)
                (*   List.fold_left *)
                (*     (fun i y -> SI.union i (do_find y)) *)
                (*     (do_find x) xs *)
                (* | Phi [] -> *)
                (*   failwith "Encountered empty Phi expression" *)

                    (* This tries to preserve strides for loop variables, but takes too long, and
                       wasn't working.
                       | Phi xs ->
                       let res = List.fold_left
                       (fun res y ->
                       try let l = find y in
                       match res with None -> Some l
                       | Some l' -> Some(SI.union l l')
                       with Not_found -> res
                       )
                       None xs
                       in
                       (match res with
                       | Some l -> l
                       | None -> raise Not_found
                       )
                    *)
                | Cast(cast, t, vl) ->
                  let f = cast_to_si_function cast in
                  f (bits_of_width t) (exp2si vl)
                | Concat (e1, e2) -> exp2si (Ast_convenience.rm_concat e)
                | Extract _ -> exp2si (Ast_convenience.rm_extract e)
                | Ite (c, e1, e2) ->
                  let csi = exp2si c in
                  if csi = SI.yes then exp2si e1
                  else if csi = SI.no then exp2si e2
                  else SI.union (exp2si e1) (exp2si e2)
                | Unknown _ -> top v
                | Let _ -> raise (Unimplemented "Let unimplemented")
                | Store _ | Load _ -> raise (Unimplemented "Memory unimplemented in this version of VSA"))
                with Unimplemented _ | Invalid_argument _ ->
                  top v
              in
              let new_si = exp2si e in
              if try VM.find v l <> new_si with Not_found -> true then (
                dprintf "adding %s = %s" (Pp.var_to_string v) (SI.to_string new_si);
                VM.add v new_si l )
                else l
            with Invalid_argument _ | Not_found ->
              l

    let edge_transfer_function _ _ _ _ = Util.id

  end

  module DF = CfgDataflow.MakeWide(DFP)

end (* module SimpleVSA *)



(** Value Sets *)
module VS =
struct
  type region = var (* FIXME? *)

  type address = region * SI.t

  type t = address list

  let global = Var.newvar "global region" (Reg 64) (* value doesn't really matter, so long as it's unique *)

  let top k = [(global, SI.top k)]
  let empty k = [(global, SI.empty k)]

  let rec width = function
    | (_, (k,_,_,_))::_ -> k
    | [] -> failwith "width: empty value set"

  let size k vs =
    BatList.reduce (+%) (List.map (fun (_,si) -> SI.size si) vs)

  let pp_address p (r, si) =
    if r == global then p "$" else p(Pp.var_to_string r);
    p " |=> ";
    p (SI.to_string si)

  let pp p = function
    | [] -> failwith "this should not happen"
    | x::xs ->
        p "(";
        pp_address p x;
        List.iter (fun x -> p ", "; pp_address p x) xs;
        p ")"

  let to_string vs =
    let b = Buffer.create 57 in
    let p = Buffer.add_string b in
    pp p vs;
    Buffer.contents b

  let kind = function
    | [] -> failwith "empty value sets not allowed"
    | [(r,si)] when r == global && si = SI.top 32 -> `Top
    | [(r,_)] when r == global -> `VSglob
    | [_] -> `VSsingle
    | _ -> `VSarb

  let single k x = [(global, SI.single k x)]
  let of_bap_int i t = [(global, SI.of_bap_int i t)]

  let remove_lower_bound = List.map (fun (r,si) -> (r, SI.remove_lower_bound si))
  let remove_upper_bound = List.map (fun (r,si) -> (r, SI.remove_upper_bound si))

  let zero k = [(global, SI.zero k)]
  let one k = [(global, SI.one k)]
  let minus_one k = [(global, SI.minus_one k)]

  let above k i = [(global, SI.above k i)]
  let below k i = [(global, SI.below k i)]
  let above_unsigned k i = [(global, SI.above_unsigned k i)]
  let below_unsigned k i = [(global, SI.below_unsigned k i)]

  let aboveeq k i = [(global, SI.aboveeq k i)]
  let beloweq k i = [(global, SI.beloweq k i)]
  let aboveeq_unsigned k i = [(global, SI.aboveeq_unsigned k i)]
  let beloweq_unsigned k i = [(global, SI.beloweq_unsigned k i)]

  let add k x y = match (x,y) with
    | ([r2,si2],[r1,si1]) when r1 == global ->
      let allow_overflow = r2 == global in
      [(r2, SI.add ~allow_overflow k si1 si2)]
    | ([r1,si1],[r2,si2]) when r1 == global ->
      let allow_overflow = r2 == global in
      [(r2, SI.add ~allow_overflow k si1 si2)]
    | ([r,si1], xs) | (xs, [r,si1]) when r == global ->
      List.map (fun (r,si) ->
        let allow_overflow = r == global in
        (r, SI.add ~allow_overflow k si1 si)) xs
    | _ -> top k

  let sub k x = function
    | [r,si] when r == global ->
      List.map (fun (r,si') -> (r, SI.sub k si' si)) x
    | _ -> top k

  let makeother f id annihilator k x y =
    match (x,y) with
    | ([r1,si1], [r2,si2]) when r1 == global && r1 == r2 ->
      [(r1, f k si1 si2)]
    | ([_] as vsg, vs) when vsg = id ->
        vs
    | (vs, ([_] as vsg))  when vsg = id ->
        vs
    | ([_] as vsg, _)  when Some vsg = annihilator ->
        BatOption.get annihilator
    | (_,([_] as vsg))  when Some vsg = annihilator ->
        BatOption.get annihilator
    | _ -> top k
      

  let logand k = makeother SI.logand (minus_one k) (Some (zero k)) k

  let logor k = makeother SI.logor (zero k) (Some (minus_one k)) k

  let logxor k = makeother SI.logxor (zero k) None k

  let si_to_vs_binop_function f k =
    let g vs1 vs2 = match vs1, vs2 with
      | [(r1,si1)], [(r2,si2)] when r1 == global && r2 == global -> [(r1, f k si1 si2)]
      | _ -> raise(Unimplemented "unimplemented binop") in
    g

  let si_to_vs_unop_function f k =
    let g vs1 = match vs1 with
      | [(r1,si1)] when r1 == global -> [(r1, f k si1)]
      | _ -> raise(Unimplemented "unimplemented unop") in
    g

  let si_to_vs_cast_function = si_to_vs_unop_function

  let concat = si_to_vs_binop_function SI.concat

  let yes = [(global, SI.yes)]
  let no = [(global, SI.no)]
  let maybe = [(global, SI.maybe)]

  (** Slightly unconservative equality checking. *)
  let eq k x y = match (x,y) with
     | ([r1,si1], [r2,si2]) when r1 == r2 ->
         [(global, SI.eq k si1 si2)]
     | (r, _) when r = top k -> maybe
     | (_, r) when r = top k -> maybe
     | _ ->
         if List.exists (fun(r,s)-> List.exists (fun(r2,s2)-> r == r2 && SI.eq k s s2 <> SI.no) y) x
         then maybe
         else no

  let equal x y =
    if x == y then true
    else x = y

  let union x y =
    let k = width x in
    if debug () then (assert (k = width y));
    if x = top k || y = top k then top k else
      let h = Hashtbl.create (List.length x + List.length y) in
      let add (r,si) =
        try Hashtbl.replace h r (SI.union (Hashtbl.find h r) si)
        with Not_found ->
          Hashtbl.add h r si
      in
      List.iter add x;
      List.iter add y;
      Hashtbl.fold (fun k v r -> (k,v)::r) h []

  let intersection x y =
    let k = width x in
    if debug () then (assert (k = width y));
    if x = top k then y
    else if y = top k then x
    else let hx = Hashtbl.create (List.length x) in
         let add (r,si) =
           Hashtbl.add hx r si
         in
         List.iter add x;
         List.fold_left
           (fun l (r,si) ->
             try (r, SI.intersection si (Hashtbl.find hx r))::l
             with Not_found -> l)
           [] y

  let widen x y =
    let k = width x in
    if debug () then (assert (k = width y));
    if x = top k || y = top k then top k else
    let h = Hashtbl.create (List.length x + List.length y) in
    let add (r,si) =
      try Hashtbl.replace h r (SI.widen (Hashtbl.find h r) si)
      with Not_found ->
        Hashtbl.add h r si
    in
      List.iter add x;
      List.iter add y;
      Hashtbl.fold (fun k v r -> (k,v)::r) h []

  let fold f vs init =
    (* if vs = top addr_bits then wprintf "VS.fold is very slow for Top"; *)
    List.fold_left (fun a (r,si) -> SI.fold (fun v -> f (r,v)) si a) init vs

  let concrete ?max vs =
    let get_value (r,o) (l,ctr) =
      (match max with
      | Some x -> if ctr > x then raise Exit
      | None -> ());
      if r == global then
        o::l, ctr+1
      else raise Exit in
    try
      let l,_ = fold get_value vs ([],1) in
      Some l
    with Exit -> None

  let numconcrete vs =
    fold (fun _ a -> a +% 1L) vs 0L
end



(* Very simplified version of VSA, supporting regions *)
module RegionVSA =
struct
  module DFP =
  struct
    module CFG = Cfg.AST
    module L =
    struct
      type t = VS.t VM.t
      let top = VM.empty
      let equal = VM.equal (=)
      let meet x y =
        VM.fold
          (fun k v res ->
             try
               let v' = VM.find k y in
               let vs = VS.union v v' in
                 VM.add k vs res
             with Not_found ->
               VM.add k v res
          )
          x y
      let widen x y =
        VM.fold
          (fun k v res ->
             try
               let v' = VM.find k y in
               let vs = VS.widen v v' in
                 VM.add k vs res
             with Not_found ->
               VM.add k v res
          )
          x y
    end
    module O = GraphDataflow.NOOPTIONS
    let s0 _ _ = CFG.G.V.create Cfg.BB_Entry
    let init _ g =
        VM.add sp [(sp, SI.zero (bits_of_width (Var.typ sp)))] L.top (* stack region *)

    let dir _ = GraphDataflow.Forward

    let binop_to_vs_function = function
      | PLUS -> VS.add
      | MINUS -> VS.sub
      | AND -> VS.logand
      | OR -> VS.logor
      | XOR -> VS.logxor
      | EQ -> VS.eq
      | TIMES
      | DIVIDE
      | SDIVIDE
      | MOD
      | SMOD
      | LSHIFT
      | RSHIFT
      | ARSHIFT
      | NEQ
      | LT
      | LE
      | SLT
      | SLE as bop
        -> VS.si_to_vs_binop_function (SimpleVSA.DFP.binop_to_si_function bop)

    let unop_to_vs_function = function
      | NEG
      | NOT as unop
        -> VS.si_to_vs_unop_function (SimpleVSA.DFP.unop_to_si_function unop)

    let cast_to_vs_function = function
      | CAST_UNSIGNED
      | CAST_SIGNED
      | CAST_HIGH
      | CAST_LOW as ct
        -> VS.si_to_vs_cast_function (SimpleVSA.DFP.cast_to_si_function ct)


    let rec stmt_transfer_function _ _ _ s l =
      match s with
        | Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
        | Assert _ | Assume _ | Jmp _ | CJmp _ | Label _ | Comment _
        | Halt _ ->
            l
        | Special _ -> L.top
        | Move(v, e, _) ->
            try
              let find v = VM.find v l in
              let do_find v = try find v with Not_found -> VS.top (bits_of_width (Var.typ v)) in
              let rec exp2vs e =
                try (match e with
                | Int(i,t) -> VS.of_bap_int (int64_of_big_int i) t
                | Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)")
                | Var v -> do_find v
                | BinOp(op, x, y) ->
                  let f = binop_to_vs_function op in
                  let k = SimpleVSA.DFP.bits_of_exp x in
                  f k (exp2vs x) (exp2vs y)
                | UnOp(op, x) ->
                  let f = unop_to_vs_function op in
                  let k = SimpleVSA.DFP.bits_of_exp x in
                  f k (exp2vs x)
                (* | Phi(x::xs) -> *)
                (*   List.fold_left *)
                (*     (fun i y -> VS.union i (do_find y)) *)
                (*     (do_find x) xs *)
                (* | Phi [] -> *)
                (*   failwith "Encountered empty Phi expression" *)
                | Cast (ct, t, x) ->
                  let f = cast_to_vs_function ct in
                  let k = Typecheck.bits_of_width t in
                  f k (exp2vs x)
                | _ ->
                  raise(Unimplemented "unimplemented expression type"))
                with Unimplemented _ | Invalid_argument _ -> VS.top (bits_of_width (Typecheck.infer_ast ~check:false e))
              in
              let new_vs = exp2vs e in
              VM.add v new_vs l
            with Invalid_argument _ | Not_found ->
              l

    let edge_transfer_function _ _ _ _ = Util.id

  end
  
  module DF = CfgDataflow.MakeWide(DFP)

end (* module RegionVSA *)

(** Abstract Store *)
module MemStore = struct
  type aloc = VS.region * int64
  module M1 = BatMap.Make(struct type t = VS.region let compare = Var.compare end)
  module M2 = BatMap.Make(struct type t = int64 let compare = Int64.compare end)

  (* VSA optional interface: specify a "real" memory read function *)
  module O = struct
    type t = { initial_mem : (addr * char) list }
    let default = { initial_mem = [] }
  end

  (** This implementation may change... *)
  type t = VS.t M2.t M1.t

  let top = M1.empty

  (** Fold over all addresses in the MemStore *)
  let fold f ae i =
    M1.fold (fun r m2 a -> M2.fold (fun i vs a -> f (r,i) vs a) m2 a) ae i

  let pp p a =
    p "Memory contents:\n";
    fold (fun (r,i) vs () ->
      let region = if r == VS.global then "$" else Pp.var_to_string r in
      p (Printf.sprintf " %s[%#Lx] -> %s\n" region i (VS.to_string vs))) a ();
    p "End contents."

  (* let read_concrete_real ?o (r,i) =match o with *)
  (*   | Some {O.get_mem=get_mem} when r = VS.global -> *)
  (*     let prog_mem = BatList.filter_map get_mem [i;i+%1L;i+%2L;i+%3L] in *)
  (*     if List.length prog_mem = 4 then *)
  (*       (\* Ugh, fix this *\) *)
  (*       let i64 = Arithmetic.bytes_to_int64 `Little prog_mem in *)
  (*       VS.of_bap_int i64 reg_32 *)
  (*     else VS.top *)
  (*   | _ -> VS.top *)

  let rec read_concrete k ?o ae (r,i) =
    try
      let v = M2.find i (M1.find r ae) in
      let w = VS.width v in
      assert (w mod 8 = 0);
      if w = k then v
      else (
        (* We wanted to read k bits, but read w instead. Let's try to
           read from i+w/8 and get the rest. *)
        if w > k then
          (* We read too many bytes: use extract *)
          VS.top k
        else
          (* We read too few bytes: use concat
             XXX: Handle address wrap-around properly
          *)
          let rest = read_concrete (k-w) ?o ae (r, i+%((Int64.of_int w)/%8L)) in
          (* XXX: Endianness *)
          (* let () = dprintf "Concatenating %Ld %s and %s ->" i (VS.to_string rest) (VS.to_string v) in *)
          VS.concat k rest v)
    with Not_found ->
      VS.top k

  let read k ?o ae = function
    | [] -> failwith "empty value sets not allowed"
    | addrs -> (* FIXME: maybe shortcut this *)
      try
        let res =
          VS.fold
            (fun v a ->
              match a with
            | None -> Some (read_concrete k ?o ae v)
            | Some a ->
              if a = VS.top k then raise Exit
              else
                Some (VS.union (read_concrete k ?o ae v) a)
            ) addrs None
        in
        match res with
        | Some x -> x
        | None -> failwith "MemStore.read impossible address"
      with Exit -> VS.top k

  let widen_region r =
    match !mem_max with
    | Some m ->
      if M2.cardinal r > m then M2.empty
      else r
    | None -> r

  let widen_mem m =
    M1.map (fun r -> widen_region r) m

  let write_concrete_strong k ae (r,i) vl =
    if vl = VS.top k then
      try
        let m2 = M1.find r ae in
        let m2' = M2.remove i m2 in
        if M2.is_empty m2' then M1.remove r ae else M1.add r m2' ae
      with Not_found -> ae
    else
      let m2 = try M1.find r ae with Not_found -> M2.empty in
      (* Don't overwrite the old value if it's the same; this wastes
         memory in the applicative data structure. *)
      if (try M2.find i m2 = vl with Not_found -> false)
      then ae
      else M1.add r (M2.add i vl m2) ae

  let write_concrete_weak k ae addr vl =
    write_concrete_strong k ae addr (VS.union vl (read_concrete k ae addr))

  let write_concrete_intersection k ae addr vl =
    write_concrete_strong k ae addr (VS.intersection vl (read_concrete k ae addr))

  let write_concrete_weak_widen k ae addr vl =
    write_concrete_strong k ae addr (VS.widen vl (read_concrete k ae addr))

  let write k ae addr vl =
    if addr = VS.top addr_bits then (
      if vl = VS.top k then top
      else match !mem_max with
      | None -> fold (fun addr v a -> write_concrete_weak k a addr vl) ae ae
      | Some _ -> top
    ) else match addr with
      | [(r, ((k,_,_,_) as o))] when o = SI.top k ->
        (* Set this entire region to Top *)
        M1.remove r ae
      | [(r, (_,0L,x,y))] when x = y ->
        write_concrete_strong k ae (r,x) vl
      | _ ->
        (match !mem_max with
        | Some m ->
          if VS.size k addr > Int64.of_int m then top
          else widen_mem (VS.fold (fun v a -> write_concrete_weak k a v vl) addr ae)
        | None -> widen_mem (VS.fold (fun v a -> write_concrete_weak k a v vl) addr ae))

  let write_intersection k ae addr vl =
    match addr with
    | [(r, (_,0L,x,y))] when x = y ->
      write_concrete_intersection k ae (r,x) vl
    | _ ->
      (* Since we don't know what location is getting the
         intersection, we can't do anything. *)
      ae

  let equal x y =
    if x == y then true
    else M1.equal (M2.equal (=)) x y

  let merge_region f x y =
    if M2.equal (=) x y then x
    else
      M2.merge (fun a v1 v2 -> match v1, v2 with
      | Some v1, Some v2 -> Some (f v1 v2)
      | Some v, None
      | None, Some v -> Some v
      | None, None -> None) x y

  let merge_mem f =
    M1.merge (fun r v1 v2 -> match v1, v2 with
    | Some v1, Some v2 -> Some (merge_region f v1 v2)
    | Some v, None
    | None, Some v -> Some v
    | None, None -> None)

  let intersection (x:t) (y:t) =
    if equal x y then x
    else merge_mem VS.intersection x y

  let union (x:t) (y:t) =
    if equal x y then x
    else merge_mem VS.union x y

  let widen (x:t) (y:t) =
    if equal x y then x
    else merge_mem VS.widen x y

end

(** Abstract Environment *)
module AbsEnv = struct

  type value = [ `Scalar of VS.t | `Array of MemStore.t ]

  (** This implementation may change *)
  type t = value VM.t

  let empty = VM.empty

  let pp_value p = function
    | `Scalar s -> VS.pp p s
    | `Array a -> MemStore.pp p a

  let value_to_string v =
    let b = Buffer.create 57 in
    let p = Buffer.add_string b in
    pp_value p v;
    Buffer.contents b

  let pp p m =
    VM.iter (fun k v ->
      p ("\n " ^ (Pp.var_to_string k) ^ " -> ");
      pp_value p v;
    ) m

  let to_string m =
    let b = Buffer.create 57 in
    let p = Buffer.add_string b in
    pp p m;
    Buffer.contents b

  let value_equal x y = match x,y with
    | (`Scalar x, `Scalar y) -> VS.equal x y
    | (`Array x, `Array y) -> MemStore.equal x y
    | _ -> failwith "value_equal"

  let equal x y =
    if x == y then true
    else VM.equal (value_equal) x y

  let do_find_vs ae v =
    try match VM.find v ae with
      | `Scalar vs -> vs
      | _ -> failwith "type mismatch" (*VS.top (bits_of_width (Var.typ v))*)
    with Not_found -> VS.top (bits_of_width (Var.typ v))

  (* let astval2vs ae = function *)
  (*   | Int(i,t) -> VS.of_bap_int (int64_of_big_int i) t *)
  (*   | Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)") *)
  (*   | Var v -> do_find_vs ae v *)

  let do_find_ae ae v =
    try match VM.find v ae with
      | `Array ae -> ae
      | _ -> failwith "Wanted to find abstract environment but found scalar. Not sure why this would happen"
    with  Not_found -> MemStore.top
end  (* module AE *)




(** This does most of VSA, except the loop handling and special dataflow *)
module AlmostVSA =
struct
  module DFP =
  struct
    module CFG = Cfg.AST
    module L =
    struct
      type t = AbsEnv.t
      let top = AbsEnv.empty
      let equal = AbsEnv.equal
      let meet (x:t) (y:t) =
        if equal x y then x
        else VM.merge
          (fun k v1 v2 -> match v1, v2 with
          | Some (`Scalar a), Some (`Scalar b) -> Some(`Scalar(VS.union a b ))
          | Some (`Array a), Some (`Array b) -> Some(`Array(MemStore.union a b))
          | Some (`Scalar _), Some (`Array _)
          | Some (`Array _), Some (`Scalar _) -> failwith "Tried to meet scalar and array"
          | (Some _ as s), None
          | None, (Some _ as s) -> s
          | None, None -> None) x y
      let widen (x:t) (y:t) =
        if equal x y then x
        else VM.merge
          (fun k v1 v2 -> match v1, v2 with
          | Some (`Scalar a), Some (`Scalar b) -> dprintf "widening %s" (Pp.var_to_string k); Some(`Scalar(VS.widen a b))
          | Some (`Array a), Some (`Array b) -> Some(`Array(MemStore.union a b))
          | Some (`Scalar _), Some (`Array _)
          | Some (`Array _), Some (`Scalar _) -> failwith "Tried to widen scalar and array"
          | (Some _ as s), None
          | None, (Some _ as s) -> s
          | None, None -> None) x y

(*      let widen x y =
        let v = widen x y in
        print_string "x\n";
        AbsEnv.pp print_string x;
        print_string "\ny\n";
        AbsEnv.pp print_string y;
        print_string "\nwiden\n";
        AbsEnv.pp print_string v;
        print_string "\n";
        v *) 
    end
    module O = MemStore.O

    let s0 _ _ = CFG.G.V.create Cfg.BB_Entry

    (** Creates a lattice element that maps each of the given variables to
        it's own region. (For use as an inital value in the dataflow problem.)
    *)
    let init_vars vars =
      List.fold_left (fun vm x -> VM.add x (`Scalar [(x, SI.zero (bits_of_width (Var.typ x)))]) vm) L.top vars

    let init_mem vm {O.initial_mem=initial_mem} =
      let write_mem m (a,v) =
        DV.dprintf "Writing %#x to %#Lx" (Char.code v) a;
        let v = Char.code v in
        let v = Int64.of_int v in
        MemStore.write 8 m (VS.single 32 a) (VS.single 8 v)
      in
      let m = List.fold_left write_mem (MemStore.top) initial_mem in
      VM.add Disasm_i386.mem (`Array m) vm

    let init o g : L.t =
      let vm = init_vars [sp] in
      init_mem vm o

    let dir _ = GraphDataflow.Forward

    let find v l = VM.find v l
    let do_find = AbsEnv.do_find_vs
    let do_find_ae = AbsEnv.do_find_ae

    (* aev = abstract environment value *)
    let rec exp2vs ?o l e =
      match exp2aev ?o l e with
      | `Scalar vs -> vs
      | _ -> failwith "exp2vs: Expected scalar"
    and exp2aev ?o l e : AbsEnv.value =
      match Typecheck.infer_ast ~check:false e with
      | Reg nbits -> (
        let new_vs = try (match e with
          | Int(i,t)->
            VS.of_bap_int (int64_of_big_int i) t
          | Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)")
          | Var v -> do_find l v
          | BinOp(op, x, y) ->
            let f = RegionVSA.DFP.binop_to_vs_function op in
            let k = SimpleVSA.DFP.bits_of_exp x in
            f k (exp2vs ?o l x) (exp2vs ?o l y)
          | UnOp(op, x) ->
            let f = RegionVSA.DFP.unop_to_vs_function op in
            let k = SimpleVSA.DFP.bits_of_exp x in
            f k (exp2vs ?o l x)
                  (* | Phi(x::xs) -> *)
                  (*   List.fold_left *)
                  (*     (fun i y -> VS.union i (do_find l y)) *)
                  (*     (do_find l x) xs *)
                  (* | Phi [] -> *)
                  (*   failwith "Encountered empty Phi expression" *)
          | Load(Var m, i, _e, t) ->
            (* FIXME: assumes deendianized.
               ie: _e and _t should be the same for all loads and
               stores of m. *)
            MemStore.read (bits_of_width t) ?o (do_find_ae l m) (exp2vs ?o l i)
          | Cast (ct, t, x) ->
            let f = RegionVSA.DFP.cast_to_vs_function ct in
            let k = Typecheck.bits_of_width t in
            f k (exp2vs ?o l x)
          | Load _ | Concat _ | Extract _ | Ite _ | Unknown _ | Let _ | Store _ ->
            raise(Unimplemented "unimplemented expression type"))
          with Unimplemented s | Invalid_argument s -> DV.dprintf "unimplemented %s %s!" s (Pp.ast_exp_to_string e); VS.top nbits
        in `Scalar new_vs
      )
      | TMem _ | Array _ -> (
        let new_vs = try (match e with
          | Var v ->
            do_find_ae l v
          | Store(Var m,i,v,_e,t) ->
                    (* FIXME: assumes deendianized.
                       ie: _e and _t should be the same for all loads and
                       stores of m. *)
            dprintf "doing a write... to %s of %s." (VS.to_string (exp2vs ?o l i)) (VS.to_string (exp2vs ?o l v));
            (* dprintf "size %#Lx" (VS.numconcrete (exp2vs ?o l i)); *)
            MemStore.write (bits_of_width t)  (do_find_ae l m) (exp2vs ?o l i) (exp2vs ?o l v)
          (* | Phi(x::xs) -> *)
          (*   List.fold_left *)
          (*     (fun i y -> MemStore.union i (do_find_ae l y)) *)
          (*     (do_find_ae l x) xs *)
          (* | Phi [] -> *)
          (*   failwith "Encountered empty Phi expression" *)
          | _ ->
            raise(Unimplemented "unimplemented memory expression type"))
          with Unimplemented _ | Invalid_argument _ -> MemStore.top
        in `Array new_vs
      )

    let rec stmt_transfer_function o _ _ s l =
      dprintf "Executing %s" (Pp.ast_stmt_to_string s);
      match s with
        | Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
        | Assert _ | Assume _ | Jmp _ | CJmp _ | Label _ | Comment _
        | Halt _ ->
            l
        | Special _ -> L.top
        | Move(v, e, _) ->
          try
            let new_vs = exp2aev ~o l e in
            if DV.debug () then
            (match new_vs with
            | `Scalar new_vs ->
              DV.dprintf "Assign %s <- %s" (Pp.var_to_string v) (VS.to_string new_vs)
            | _ -> ());
            VM.add v new_vs l
          with Invalid_argument _ | Not_found ->
            l

    let edge_transfer_function o g edge _ l =
      dprintf "edge from %s to %s" (Cfg_ast.v2s (Cfg.AST.G.E.src edge)) (Cfg_ast.v2s (Cfg.AST.G.E.dst edge));
      let accept_signed_bop bop =
        match !signedness_hack, bop with
        | false, (SLE|SLT) -> true
        | true, (SLE|SLT|LE|LT) -> true
        | _, _ -> false
      in
      match CFG.G.E.label edge with
      (* Because strided intervals represent signed numbers, we
         cannot convert unsigned inequalities to strided intervals (try
         it). *)
      | Some(_, BinOp(EQ, (BinOp((SLE|SLT|LE|LT) as bop, Var v, Int(i, t)) as be), Int(i', t')))
      | Some(_, BinOp(EQ, (BinOp((SLE|SLT|LE|LT) as bop, Int(i, t), Var v) as be), Int(i', t')))
          when accept_signed_bop bop ->

        let dir = match be with
          | BinOp(_, Var _, Int _) -> `Below
          | BinOp(_, Int _, Var _) -> `Above
          | _ -> failwith "impossible"
        in

        (* Reverse if needed *)
        let e, dir, bop =
          if bi_is_one i' then be, dir, bop
          else
            let newbop = match bop with
              | SLE -> SLT
              | SLT -> SLE
              | LE -> LT
              | LT -> LE
              | _ -> failwith "impossible"
            in
            match dir with
            | `Below -> BinOp(newbop, Int(i, t), Var v), `Above, newbop
            | `Above -> BinOp(newbop, Var v, Int(i, t)), `Below, newbop
        in
        let vsf = match dir, bop with
          | `Below, SLE -> VS.beloweq
          | `Below, LE -> VS.beloweq_unsigned
          | `Below, SLT -> VS.below
          | `Below, LT -> VS.below_unsigned
          | `Above, SLE -> VS.aboveeq
          | `Above, LE -> VS.aboveeq_unsigned
          | `Above, SLT -> VS.above
          | `Above, LT -> VS.above_unsigned
          | _ -> failwith "impossible"
        in
        let vs_v = do_find l v in
        let vs_c = vsf (bits_of_width t) (int64_of_big_int i) in
        let vs_int = VS.intersection vs_v vs_c in
        dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.var_to_string v) (Cfg_ast.v2s (CFG.G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int);
        VM.add v (`Scalar vs_int) l
      | Some(_, BinOp(EQ, (BinOp((SLE|SLT|LE|LT) as bop, (Load(Var m, ind, _e, t) as le), Int(i, t')) as be), Int(i', t'')))
      | Some(_, BinOp(EQ, (BinOp((SLE|SLT|LE|LT) as bop, Int(i, t'), (Load(Var m, ind, _e, t) as le)) as be), Int(i', t'')))
          when accept_signed_bop bop ->
        let dir = match be with
          | BinOp(_, Load _, Int _) -> `Below
          | BinOp(_, Int _, Load _) -> `Above
          | _ -> failwith "impossible"
        in

        (* Reverse if needed *)
        let e, dir, bop =
          if bi_is_one i' then be, dir, bop
          else
            let newbop = match bop with
              | SLE -> SLT
              | SLT -> SLE
              | LT -> LE
              | LE -> LT
              | _ -> failwith "impossible"
            in
            match dir with
            | `Below -> BinOp(newbop, Int(i, t), Load(Var m, ind, _e, t)), `Above, newbop
            | `Above -> BinOp(newbop, Load(Var m, ind, _e, t), Int(i, t)), `Below, newbop
        in
        let vsf = match dir, bop with
          | `Below, SLE -> VS.beloweq
          | `Below, LE -> VS.beloweq_unsigned
          | `Below, SLT -> VS.below
          | `Below, LT -> VS.below_unsigned
          | `Above, SLE -> VS.aboveeq
          | `Above, LE -> VS.aboveeq_unsigned
          | `Above, SLT -> VS.above
          | `Above, LT -> VS.above_unsigned
          | _ -> failwith "impossible"
        in
        let vs_v = exp2vs ~o l le in
        let vs_c = vsf (bits_of_width t) (int64_of_big_int i) in
        let vs_int = VS.intersection vs_v vs_c in
        dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.var_to_string m) (Cfg_ast.v2s (CFG.G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int);
        let orig_mem = do_find_ae l m in
        let new_mem = MemStore.write_intersection (bits_of_width t) orig_mem (exp2vs l ind) vs_int in
        VM.add m (`Array new_mem) l
      | Some(_, BinOp(EQ, (BinOp(EQ|NEQ as bop, Var v, Int(i, t))), Int(i', t')))
      | Some(_, BinOp(EQ, (BinOp(EQ|NEQ as bop, Int(i, t), Var v)), Int(i', t'))) ->

        (* We can make a SI for equality, but not for not for
           inequality *)
        let vs_c =
          let s = VS.of_bap_int (int64_of_big_int i) t in
          match bop with
          | EQ when i' = bi1 -> s
          | NEQ when i' = bi0 -> s
          | _ -> VS.top (bits_of_width t)
        in

        let vs_v = do_find l v in
        let vs_int = VS.intersection vs_v vs_c in
        dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.var_to_string v) (Cfg_ast.v2s (CFG.G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int);
        VM.add v (`Scalar vs_int) l

      | Some(_, BinOp((SLT|SLE), Var v2, Var v1)) ->
        (* XXX: Can we do something different for SLT? *)
        let vs_v1 = do_find l v1
        and vs_v2 = do_find l v2 in
        let vs_lb = VS.remove_upper_bound vs_v2
        and vs_ub = VS.remove_lower_bound vs_v1 in
        let vs_v1 = VS.intersection vs_v1 vs_lb
        and vs_v2 = VS.intersection vs_v2 vs_ub in
        let l = VM.add v1 (`Scalar vs_v1) l in
        VM.add v2 (`Scalar vs_v2) l
      | Some(_, e) -> dprintf "no edge match %s" (Pp.ast_exp_to_string e); l
      | _ -> l

  end

  module DF = CfgDataflow.MakeWide(DFP)

end
