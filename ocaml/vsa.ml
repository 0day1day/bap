(** Value-Set Analysis / Value-Set Arithmetic

    See Gogul Balakrishnan's thesis at
    http://pages.cs.wisc.edu/~bgogul/Research/Thesis/thesis.html

    TODO:
    * Consolidate widen/meet/union functions
    * Remove extra "k" arguments
    * Handle specials: map everything to Top
    * Add a real interface; automatically call simplify_cond
    * Big int support
    * Handle signedness
    * Handle full/partial memory correctly
    * Idea: Use copy propagation information to maintain equivalence classes, and use intersection over equivalence class members at edge transfer
*)

module VM = Var.VarMap

open Big_int_convenience
open Big_int_Z
open Util
open Type
open Ast

module D = Debug.Make(struct let name = "VSA" and default=`NoDebug end)
open D

(* Treat unsigned comparisons the same as signed: should be okay as
   long as overflow does not occur *)
let signedness_hack = ref true

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

let fwd_transfer_stmt_to_block f g node latice =
  List.fold_left (fun l n -> f n l) latice (Cfg.AST.get_stmts g node)

(* FIXME: find a better way to get the stack pointer *)
let sp = Disasm_i386.esp


(** Strided Intervals *)
module SI =
struct
(* FIXME: some of these functions can return a stride of 1 for a singleton *)

  (** number of bits * unsigned stride * signed lower bound * signed upper bound *)
  type t = int * int64 * int64 * int64

  let to_string (_,s,lb,ub) =
    if s = (-1L) then "[]"
    else Printf.sprintf "%Lu[%Ld,%Ld]" s lb ub

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

  let remove_lower_bound (k,s,a,b) =
    (k,s,mini k,b)
  let remove_upper_bound (k,s,a,b) =
    (k,s,a,maxi k)

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

  let is_empty (k,s,lb,ub) =
    s = (-1L) && lb = 1L && ub = 0L

  (* XXX: Remove k argument *)
  let is_reduced k ((k',s,lb,ub) as si) =
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
      check_reduced k si';
      si'

  let renormbin f k x y = renorm k (f k x y)
  let renormun f k x = renorm k (f k x)

  (* XXX: Remove k *)
  (** Addition of strided intervals *)
  let add k ((k',s1,lb1,ub1) as a) ((k'',s2,lb2,ub2) as b) =
    assert (k=k' && k=k'');
    check_reduced2 k a b;
    let lb' = lb1 +% lb2 
    and ub' = ub1 +% ub2 in
    let u = lb1 &% lb2 &% bnot lb' &% bnot(ub1 &% ub2 &% bnot ub')
    and v = ((lb1 ^% lb2) |% bnot(lb1 ^% lb'))
      &% (bnot ub1 &% bnot ub2 &% ub')
    and highbit = highbit k
    in
      if (u |% v) &% highbit = highbit then
        top k
      else (k, uint64_gcd s1 s2, extend k lb', extend k ub')

  let add = renormbin add

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

  let neg = if debug() then renormun neg else neg
        
  (** Subtractionf of strided intervals *)
  let sub k a b =
    add k a (neg k b)


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

  let lognot = if debug() then renormun lognot else lognot


  (** Bitwise AND *)
  let logand k x y =
    lognot k (logor k (lognot k x) (lognot k y))

  (** Bitwise XOR *)
  let logxor k x y =
    let n = lognot k
    and o = logor k in
    o (n(o (n x) y)) (n(o x (n y)))

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
    (* Shift lower bound of x by minimum shift amount *)
    let a1 = shift aa z1
    (* Shift lower bound of x by maximum shift amount *)
    and a2 = shift aa z2
    (* Shift upper bound of x by minimum shift amount *)
    and b1 = shift bb z1
    (* Shift upper bound of x by maximum shift amount *)
    and b2 = shift bb z2
    (* Compute new stride info. *)
    and s' = match dir with
      | `Rightshift -> int64_umax (Int64.shift_right_logical s1 z2) 1L
      | `Leftshift -> int64_umax (Int64.shift_left s1 z2) 1L
    in
    (* Finally pick the lower and upper bounds.  All the values above
       are checked because of sign overflow. *)
    let l = min (min a1 a2) (min b1 b2)
    and u = max (max a1 a2) (max b1 b2) in
    renorm k (k,s',l,u)

  (** Logical right-shift *)
  let rshift = mk_shift `Rightshift Int64.shift_right_logical

  (** Arithmetic right-shift *)
  let arshift = mk_shift `Rightshift Int64.shift_right

  (** Left shift *)
  let lshift = mk_shift `Leftshift Int64.shift_left

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
        else (k, 1L, l, u)

  let union x y =
    let (k,a,b,c) as res = union x y in
      if b = c then (k,0L,b,b) else (check_reduced k res; res)

  let intersection (k,s1,a,b) (k',s2,c,d) =
    if k <> k' then failwith "intersection: expected same bitwidth intervals";
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
      else
        (k, 1L, l, u))

  let widen (k,s1,a,b) (k',s2,c,d) =
    if k <> k' then failwith "widen: expected same bitwidth intervals";
    let s' = uint64_gcd s1 s2 in
    let l = if c < a then mini k else min a c
    and u = if d > b then maxi k else max b d in
    if s' = 0L then
      if a = b && c = d then
        (k, u -% l, l, u)
      else failwith "widen: strided interval not in reduced form"
    else (k, 1L, l, u)

  let rec fold f (k,s,a,b) init =
    if a = b then f a init
    else fold f (k, s, a+%s ,b) (f a init)

end (* module SI *)

(* Very simplified version of VSA, with no bounding *)
module SimpleVSA =
struct
  module DFP =
  struct
    module G = Cfg.AST.G
    module L =
    struct
      type t = SI.t VM.t
      let top = VM.empty
      let equal = VM.equal (=)
      let meet x y =
        VM.fold
          (fun k v res ->
             try
               let v' = VM.find k y in
               let si = SI.union v v' in
                 VM.add k si res
             with Not_found ->
               VM.add k v res
          )
          x y
      let widen x y =
        VM.fold
          (fun k v res ->
             try
               let v' = VM.find k y in
               let si = SI.widen v v' in
                 VM.add k si res
             with Not_found ->
               VM.add k v res
          )
          x y
    end
    let s0 _ = G.V.create Cfg.BB_Entry
    let init g = L.top
    let dir = GraphDataflow.Forward

    let binop_to_si_function = function
      | PLUS -> SI.add
      | MINUS -> SI.sub
      | AND -> SI.logand
      | OR -> SI.logor
      | XOR -> SI.logxor
      | MOD -> SI.modulus
      | RSHIFT -> SI.rshift
      | ARSHIFT -> SI.arshift
      | LSHIFT -> SI.lshift
      | EQ -> SI.eq
      | NEQ -> fun k x y -> SI.lognot k (SI.eq k x y)
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

    let bits_of_exp e = bits_of_width (Typecheck.infer_ast ~check:false e)

    let rec transfer_stmt s l =
      match s with
        | Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
        | Assert _ | Jmp _ | CJmp _ | Label _ | Comment _
        | Halt _ ->
            l
        | Special _ -> failwith "Specials are unimplemented"
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
                | Cast(CAST_SIGNED, _, vl) ->
                  exp2si vl
                    (* This can result in non-reduced SIs currently 
                       | Cast(CAST_UNSIGNED, t, vl) ->
                       let k = bits_of_val vl
                       and k' = bits_of_width t
                       and (s,a,b) as si = val2si vl in
                       let d = 64-k in
                       let f x = I.shift_right_logical(I.shift_left x d) d in
                       if k <> 64 && k <> k' then
                       (s, f a, f b)
                       else si
                    *)
                | Cast(CAST_HIGH, t, vl) ->
                  let k = bits_of_exp vl
                  and k' = bits_of_width t
                  and (k'',s,a,b) as si = exp2si vl in
                  assert (k=k'');
                  let f x = I.shift_right x (k' - k) in
                  if k' <> k then
                    (k', s, f a, f b)
                  else si
                | Cast(CAST_LOW, t, vl) ->
                  raise(Unimplemented "FIXME")
                | _ ->
                  raise(Unimplemented "unimplemented expression type"))
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

    let node_transfer_function = fwd_transfer_stmt_to_block transfer_stmt

    let edge_transfer_function g e = Util.id

  end
  
  module DF = GraphDataflow.MakeWide(DFP)

end (* module SimpleVSA *)



(** Value Sets *)
module VS =
struct
  type region = var (* FIXME? *)

  type address = region * SI.t

  type t = address list
      (* [] = (top, top, top,...), otherwise, any region not in the list
         maps to bottom. *)

  let global = Var.newvar "internal only" (Reg 64) (* value doesn't really matter, so long as it's unique *)

  let top = []

  let pp_address p (r, si) =
    if r == global then p "$" else p(Pp.var_to_string r);
    p " |=> ";
    p (SI.to_string si)

  let pp p = function
    | [] -> p "VS.top"
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
    | [] -> `Top
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
        [(r2, SI.add k si1 si2)]
    | ([r,si1], xs) | (xs, [r,si1]) when r == global ->
        List.map (fun (r,si) -> (r, SI.add k si1 si)) xs
    | _ -> top

  let sub k x = function
    | [r,si] when r == global ->
        List.map (fun (r,si') -> (r, SI.sub k si' si)) x
    | _ -> top

        
  let makeother f id annihilator  k x y = match (x,y) with
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
    | _ -> top

  let logand k = makeother SI.logand (minus_one k) (Some (zero k)) k

  let logor k = makeother SI.logor (zero k) (Some (minus_one k)) k

  let logxor k = makeother SI.logxor (zero k) None k

  let yes = [(global, SI.yes)]
  let no = [(global, SI.no)]
  let maybe = [(global, SI.maybe)]

  (** Slightly unconservative equality checking. *)
  let eq k x y = match (x,y) with
     | ([r1,si1], [r2,si2]) when r1 == r2 ->
         [(global, SI.eq k si1 si2)]
     | ([], _) | (_,[]) -> maybe
     | _ ->
         if List.exists (fun(r,s)-> List.exists (fun(r2,s2)-> r == r2 && SI.eq k s s2 <> SI.no) y) x
         then maybe
         else no

  let union x y =
    if x = top || y = top then top else
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
    if x = top then y
    else if y = top then x
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
    if x = top || y = top then top else
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
    if vs = top then failwith "VS.fold doesn't work on Top"
    else
      List.fold_left (fun a (r,si) -> SI.fold (fun v -> f (r,v)) si a) init vs
end



(* Very simplified version of VSA, supporting regions *)
module RegionVSA =
struct
  module DFP =
  struct
    module G = Cfg.AST.G
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
    let s0 _ = G.V.create Cfg.BB_Entry
    let init g =
        VM.add sp [(sp, SI.zero (bits_of_width (Var.typ sp)))] L.top (* stack region *)

    let dir = GraphDataflow.Forward

    let si_to_vs_binop_function f k =
      let g vs1 vs2 = match vs1, vs2 with
        | [(r1,si1)], [(r2,si2)] when r1 == VS.global && r2 == VS.global -> [(r1, f k si1 si2)]
        | _ -> raise(Unimplemented "unimplemented binop") in
      g

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
        -> si_to_vs_binop_function (SimpleVSA.DFP.binop_to_si_function bop)

    let unop_to_vs_function _ = (raise(Unimplemented "unop") : int -> VS.t -> VS.t)


    let rec transfer_stmt s l =
      match s with
        | Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
        | Assert _ | Jmp _ | CJmp _ | Label _ | Comment _
        | Halt _ ->
            l
        | Special _ -> failwith "Special is unimplemented"
        | Move(v, e, _) ->
            try
              let find v = VM.find v l in
              let do_find v =  try find v with Not_found -> VS.top in
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
                | Cast _ ->
                  raise(Unimplemented "FIXME")
                | _ ->
                  raise(Unimplemented "unimplemented expression type"))
                with Unimplemented _ | Invalid_argument _ -> VS.top
              in
              let new_vs = exp2vs e in
              VM.add v new_vs l
            with Invalid_argument _ | Not_found ->
              l

    let node_transfer_function = fwd_transfer_stmt_to_block transfer_stmt

    let edge_transfer_function g e = Util.id

  end
  
  module DF = GraphDataflow.MakeWide(DFP)

end (* module RegionVSA *)

(** Abstract Store *)
module AllocEnv = struct
  type aloc = VS.region * int64
  module M1 = Map.Make(struct type t = VS.region let compare = Var.compare end)
  module M2 = Map.Make(struct type t = int64 let compare = Int64.compare end)

  (** This implementation may change... *)
  type t = VS.t M2.t M1.t

  let top = M1.empty

  (** Fold over all addresses in the AllocEnv *)
  let fold f ae i =
    M1.fold (fun r m2 a -> M2.fold (fun i vs a -> f (r,i) vs a) m2 a) ae i

  let pp p a =
    p "Memory contents:\n";
    fold (fun (r,i) vs () ->
      p (Printf.sprintf " %s[%#Lx] -> %s\n" (Pp.var_to_string r) i (VS.to_string vs))) a ();
    p "End contents."

  let read_concrete ae (r,i) =
    try M2.find i (M1.find r ae)
    with Not_found -> VS.top

  let read ae = function
    | [] -> VS.top
    | addrs -> (* FIXME: maybe shortcut this *)
        let res =
          VS.fold
            (fun v a ->  match a with
               | None -> Some (read_concrete ae v)
               | Some a -> Some (VS.union (read_concrete ae v) a))
            addrs None
        in
          match res with
            | Some x -> x
            | None -> failwith "AllocEnv.read impossible address"

  let write_concrete_strong ae (r,i) vl =
    if vl = VS.top then
      try
        let m2 = M1.find r ae in
        let m2' = M2.remove i m2 in
        if M2.is_empty m2' then M1.remove r ae else M1.add r m2' ae
      with Not_found -> ae
    else
      let m2 = try M1.find r ae with Not_found -> M2.empty in
        M1.add r (M2.add i vl m2) ae

  let write_concrete_weak ae addr vl =
    write_concrete_strong ae addr (VS.union vl (read_concrete ae addr))

  let write_concrete_intersection ae addr vl =
    write_concrete_strong ae addr (VS.intersection vl (read_concrete ae addr))

  let write_concrete_weak_widen ae addr vl =
    write_concrete_strong ae addr (VS.widen vl (read_concrete ae addr))

  let write ae addr vl =
    if addr = VS.top then
      if vl = VS.top then top
      else
        fold (fun k v a -> write_concrete_strong a k (VS.union vl v)) ae ae
    else
      match addr with
        | [(r, (_,0L,x,y))] when x = y ->
           write_concrete_strong ae (r,x) vl
        | _ ->
          VS.fold (fun v a -> write_concrete_weak a v vl) addr ae

  let write_intersection ae addr vl =
    match addr with
    | [(r, (_,0L,x,y))] when x = y ->
      write_concrete_intersection ae (r,x) vl
    | _ ->
      (* Since we don't know what location is getting the
         intersection, we can't do anything. *)
      ae

  let intersection (x:t) (y:t) =
    fold (fun k v res -> write_concrete_intersection res k v) x y

  let union (x:t) (y:t) =
    fold (fun k v res -> write_concrete_weak res k v) x y

  let widen (x:t) (y:t) =
    fold (fun k v res -> write_concrete_weak_widen res k v) x y

  let equal = M1.equal (M2.equal (=))

end

(** Abstract Environment *)
module AbsEnv = struct

  type value = [ `Scalar of VS.t | `Array of AllocEnv.t ]

  (** This implementation may change *)
  type t = value VM.t

  let empty = VM.empty

  let pp_value p = function
    | `Scalar s -> VS.pp p s
    | `Array a -> AllocEnv.pp p a

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
    | (`Scalar x, `Scalar y) -> x = y
    | (`Array x, `Array y) -> AllocEnv.equal x y
    | _ -> false

  let equal x y =
    VM.equal (value_equal) x y

  let do_find_vs ae v =
    try match VM.find v ae with
      | `Scalar vs -> vs
      | _ -> VS.top
    with Not_found -> VS.top

  (* let astval2vs ae = function *)
  (*   | Int(i,t) -> VS.of_bap_int (int64_of_big_int i) t *)
  (*   | Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)") *)
  (*   | Var v -> do_find_vs ae v *)

  let do_find_ae ae v =
    try match VM.find v ae with
      | `Array ae -> ae
      | _ -> AllocEnv.top
    with  Not_found -> AllocEnv.top
end  (* module AE *)




(** This does most of VSA, except the loop handling and special dataflow *)
module AlmostVSA =
struct
  module DFP =
  struct
    module G = Cfg.AST.G
    module L =
    struct
      type t = AbsEnv.t
      let top = AbsEnv.empty
      let equal = AbsEnv.equal
      let meet (x:t) (y:t) =
          VM.fold
            (fun k v res ->
               try
                 let v' = VM.find k y in
                 let vs = match v, v' with
                   | (`Scalar a, `Scalar b) -> `Scalar(VS.union a b)
                   | (`Array a, `Array b) -> `Array(AllocEnv.union a b)
                   | _ -> failwith "Tried to meet scalar and array"
                 in
                   VM.add k vs res
               with Not_found ->
                 VM.add k v res
            )
            x y
      let widen (x:t) (y:t) =
          VM.fold
            (fun k v res ->
               try
                 let v' = VM.find k y in
                 let vs = match v, v' with
                   | (`Scalar a, `Scalar b) -> `Scalar(VS.widen a b)
                   | (`Array a, `Array b) -> `Array(AllocEnv.widen a b)
                   | _ -> failwith "Tried to meet scalar and array"
                 in
                   VM.add k vs res
               with Not_found ->
                 VM.add k v res
            )
            x y

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
    let s0 _ = G.V.create Cfg.BB_Entry
      
    (** Creates a lattice element that maps each of the given variables to
        it's own region. (For use as an inital value in the dataflow problem.)
    *)
    let init_vars vars =
      List.fold_left (fun vm x -> VM.add x (`Scalar [(x, SI.zero (bits_of_width (Var.typ x)))]) vm) L.top vars

    let init g =
      init_vars [sp]

    let dir = GraphDataflow.Forward

    let find v l = VM.find v l
    let do_find = AbsEnv.do_find_vs
    let do_find_ae = AbsEnv.do_find_ae

    (* aev = abstract environment value *)
    let rec exp2vs l e =
      match exp2aev l e with
      | `Scalar vs -> vs
      | _ -> failwith "exp2vs: Expected scalar"
    and exp2aev l e : AbsEnv.value =
      match Typecheck.infer_ast ~check:false e with
      | Reg _ -> (
        let new_vs = try (match e with
          | Int(i,t)->
            VS.of_bap_int (int64_of_big_int i) t
          | Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)")
          | Var v -> do_find l v
          | BinOp(op, x, y) ->
            let f = RegionVSA.DFP.binop_to_vs_function op in
            let k = SimpleVSA.DFP.bits_of_exp x in
            f k (exp2vs l x) (exp2vs l y)
          | UnOp(op, x) ->
            let f = RegionVSA.DFP.unop_to_vs_function op in
            let k = SimpleVSA.DFP.bits_of_exp x in
            f k (exp2vs l x)
                  (* | Phi(x::xs) -> *)
                  (*   List.fold_left *)
                  (*     (fun i y -> VS.union i (do_find l y)) *)
                  (*     (do_find l x) xs *)
                  (* | Phi [] -> *)
                  (*   failwith "Encountered empty Phi expression" *)
          | Load(Var m, i, _e, _t) ->
                    (* FIXME: assumes deendianized.
                       ie: _e and _t should be the same for all loads and
                       stores of m. *)
            AllocEnv.read (do_find_ae l m) (exp2vs l i)
          | Cast _ ->
            raise(Unimplemented "FIXME")
          | _ ->
            raise(Unimplemented "unimplemented expression type"))
          with Unimplemented _ | Invalid_argument _ -> VS.top
        in `Scalar new_vs
      )
      | TMem _ | Array _ -> (
        let new_vs = try (match e with
          | Var v ->
            do_find_ae l v
          | Store(Var m,i,v,_e,_t) ->
                    (* FIXME: assumes deendianized.
                       ie: _e and _t should be the same for all loads and
                       stores of m. *)
            AllocEnv.write (do_find_ae l m) (exp2vs l i) (exp2vs l v)
          (* | Phi(x::xs) -> *)
          (*   List.fold_left *)
          (*     (fun i y -> AllocEnv.union i (do_find_ae l y)) *)
          (*     (do_find_ae l x) xs *)
          (* | Phi [] -> *)
          (*   failwith "Encountered empty Phi expression" *)
          | _ ->
            raise(Unimplemented "unimplemented memory expression type"))
          with Unimplemented _ | Invalid_argument _ -> AllocEnv.top
        in `Array new_vs
      )

    let rec transfer_stmt s l =
      match s with
        | Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
        | Assert _ | Jmp _ | CJmp _ | Label _ | Comment _
        | Halt _ ->
            l
        | Special _ -> failwith "Special is unimplemented"
        | Move(v, e, _) ->
          try
            let new_vs = exp2aev l e in
            VM.add v new_vs l
          with Invalid_argument _ | Not_found ->
            l

    let node_transfer_function = fwd_transfer_stmt_to_block transfer_stmt

    let edge_transfer_function g edge l =
      let accept_signed_bop bop =
        match !signedness_hack, bop with
        | false, (SLE|SLT) -> true
        | true, (SLE|SLT|LE|LT) -> true
        | _, _ -> false
      in
      match G.E.label edge with
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
        dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.var_to_string v) (Cfg_ast.v2s (G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int);
        VM.add v (`Scalar vs_int) l
      (* | Some(_, BinOp(EQ, (BinOp((SLE|SLT) as bop, (Load(Var m, ind, _e, t) as le), Int(i, t')) as be), Int(i', t''))) *)
      (* | Some(_, BinOp(EQ, (BinOp((SLE|SLT) as bop, Int(i, t'), (Load(Var m, ind, _e, t) as le)) as be), Int(i', t''))) -> *)
      (*   let dir = match be with *)
      (*     | BinOp(_, Load _, Int _) -> `Below *)
      (*     | BinOp(_, Int _, Load _) -> `Above *)
      (*     | _ -> failwith "impossible" *)
      (*   in *)

      (*   (\* Reverse if needed *\) *)
      (*   let e, dir, bop = *)
      (*     if bi_is_one i' then be, dir, bop *)
      (*     else *)
      (*       let newbop = match bop with *)
      (*         | SLE -> SLT *)
      (*         | SLT -> SLE *)
      (*         | LT -> LE *)
      (*         | _ -> failwith "impossible" *)
      (*       in *)
      (*       match dir with *)
      (*       | `Below -> BinOp(newbop, Int(i, t), Load(Var m, ind, _e, t)), `Above, newbop *)
      (*       | `Above -> BinOp(newbop, Load(Var m, ind, _e, t), Int(i, t)), `Below, newbop *)
      (*   in *)
      (*   let vsf = match dir, bop with *)
      (*     | `Below, (SLE|LE) -> VS.beloweq *)
      (*     | `Below, (SLT|LT) -> VS.below *)
      (*     | `Above, (SLE|LE) -> VS.aboveeq *)
      (*     | `Above, (SLT|LT) -> VS.above *)
      (*     | _ -> failwith "impossible" *)
      (*   in *)
      (*   let vs_v = exp2vs l le in *)
      (*   let vs_c = vsf (bits_of_width t) (int64_of_big_int i) in *)
      (*   let vs_int = VS.intersection vs_v vs_c in *)
      (*   dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.var_to_string m) (Cfg_ast.v2s (G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int); *)
      (*   let orig_mem = do_find_ae l m in *)
      (*   let new_mem = AllocEnv.write_intersection orig_mem (exp2vs l ind) vs_int in *)
      (*   VM.add m (`Array new_mem) l *)
      | Some(_, BinOp(EQ, (BinOp(EQ|NEQ as bop, Var v, Int(i, t))), Int(i', t')))
      | Some(_, BinOp(EQ, (BinOp(EQ|NEQ as bop, Int(i, t), Var v)), Int(i', t'))) ->

        (* We can make a SI for equality, but not for not for
           inequality *)
        let vs_c =
          let s = VS.of_bap_int (int64_of_big_int i) t in
          match bop with
          | EQ when i' = bi1 -> s
          | NEQ when i' = bi0 -> s
          | _ -> VS.top
        in

        let vs_v = do_find l v in
        let vs_int = VS.intersection vs_v vs_c in
        dprintf "%s dst %s vs_v %s vs_c %s vs_int %s" (Pp.var_to_string v) (Cfg_ast.v2s (G.E.dst edge)) (VS.to_string vs_v) (VS.to_string vs_c) (VS.to_string vs_int);
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

  module DF = GraphDataflow.MakeWide(DFP)

end
