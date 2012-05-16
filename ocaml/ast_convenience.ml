(** Utility functions for ASTs.  It's useful to have these in a
    separate file so it can use functions from Typecheck and elsewhere. *)

open Ast
open BatPervasives
open Big_int_Z
open Big_int_convenience
open Type
open Typecheck

(** Create a single target cjmp. Uses a hopefully-unique label for the other. *)
let cjmp c t =
  let l = newlab ~pref:"nocjmp" () in
  CJmp(c, t, exp_of_lab l, [])
  :: Label(l, [])
  :: []

(** Create a single target cjmp with inverted condition.  Uses a hopefully-unique label for the other. *)
let ncjmp c t =
  let l = newlab ~pref:"nocjmp" () in
  CJmp(c, exp_of_lab l, t, [])
  :: Label(l, [])
  :: []

(* exp helpers *)
let unknown t s =
  Unknown(s, t)

let binop op a b = match (a,b) with
  | (Int(a, at), Int(b, bt)) ->
    assert (at = bt);
    let (i,t) = Arithmetic.binop op (a,at) (b,bt) in
    Int(i,t)
  | _ -> BinOp(op, a, b)

let unop op a = match a with
  | Int(a, at) ->
      let (i,t) = Arithmetic.unop op (a,at) in
      Int(i,t)
  | _ -> UnOp(op, a)

let concat a b = match a,b with
  | Int(a, at), Int(b, bt) ->
    let (i,t) = Arithmetic.concat (a,at) (b,bt) in
    Int(i,t)
  | _ -> Concat(a, b)

let extract h l e = match e with
  | Int(i, t) ->
    let (i,t) = Arithmetic.extract h l (i,t) in
    Int(i,t)
  | _ -> Extract(h, l, e)

let ( +* ) a b   = binop PLUS a b
let ( -* ) a b   = binop MINUS a b
let ( ** ) a b   = binop TIMES a b
let ( <<* ) a b  = binop LSHIFT a b
let ( >>* ) a b  = binop RSHIFT a b
let ( >>>* ) a b = binop ARSHIFT a b
let ( &* ) a b   = binop AND a b
let ( |* ) a b   = binop OR a b
let ( ^* ) a b   = binop XOR a b
let ( ==* ) a b  = binop EQ a b
let ( <>* ) a b  = binop NEQ a b
let ( <* ) a b   = binop LT a b
let ( >* ) a b   = binop LT b a
let (<=* ) a b   = binop LE a b
let (>=* ) a b   = binop LE b a
(** bitwise equality *)
let ( =* ) a b   = binop XOR a (unop NOT b)

let ( ++* ) a b   = concat a b
let ( %* ) a b = binop MOD a b
let ( $%* ) a b = binop SMOD a b
let ( /* ) a b = binop DIVIDE a b
let ( $/* ) a b = binop SDIVIDE a b

let cast ct tnew = function
  | Int(i,t) -> let (i',t') = Arithmetic.cast ct (i,t) tnew in
                Int(i',t')
  | e -> Cast(ct, tnew, e)

let cast_low = cast CAST_LOW
let cast_high = cast CAST_HIGH
let cast_signed = cast CAST_SIGNED
let rec cast_unsigned tnew = function
  | Int(i,t) -> let (i',t') = Arithmetic.cast CAST_UNSIGNED (i,t) tnew in
                Int(i',t')
  | Cast(CAST_UNSIGNED, Reg t', e) when Arithmetic.bits_of_width tnew >= t' ->
    (* Recurse, since we might be able to simplify e further now *)
    cast_unsigned tnew e
  | e ->
    Cast(CAST_UNSIGNED, tnew, e)

let exp_ite ?t b e1 e2 =
  (* type inference shouldn't be needed when t is specified, but we're paranoid *)
  let tb = Typecheck.infer_ast ~check:false b in
  let t1 = Typecheck.infer_ast ~check:false e1 in
  let t2 = Typecheck.infer_ast ~check:false e2 in
  assert (t1 = t2);
  assert (tb = Reg(1));

  (match t with
    | None -> ()
    | Some t -> assert (t=t1));

  if b = exp_true then e1
  else if b = exp_false then e2
  else Ite(b, e1, e2)

let parse_ite = function
  | BinOp(OR,
	  BinOp(AND, Cast(CAST_SIGNED, _, b1), e1),
	  BinOp(AND, Cast(CAST_SIGNED, _, UnOp(NOT, b2)), e2)
  )
  | BinOp(OR,
	  BinOp(AND, b1, e1),
	  BinOp(AND, UnOp(NOT, b2), e2)
  ) when full_exp_eq b1 b2 && Typecheck.infer_ast ~check:false b1 = Reg(1) ->
    Some(b1, e1, e2)
      (* In case one branch is optimized away *)
  | BinOp(AND,
	  Cast(CAST_SIGNED, nt, b1),
	  e1) when Typecheck.infer_ast ~check:false b1 = Reg(1) ->
    Some(b1, e1, Int(zero_big_int, nt))
  | _ -> None

(** Duplicate any shared nodes. Useful for using physical location as
    a unique identity.

    XXX: I think this would be much faster if we only duplicated
    things that actually occur more than once.
*)
let rec rm_duplicates e =
  let r = rm_duplicates in
  let newe = match e with
  | Load(e1, e2, e3, t) -> Load(r e1, r e2, r e3, t)
  | Store(e1, e2, e3, e4, t) -> Store(r e1, r e2, r e3, r e4, t)
  | BinOp(bt, e1, e2) -> BinOp(bt, r e1, r e2)
  | UnOp(ut, e) -> UnOp(ut, r e)
  | Var(v) -> Var(v)
  | Lab(s) -> Lab(s)
  | Int(i, t) -> Int(i, t)
  | Cast(ct, t, e) -> Cast(ct, t, r e)
  | Let(v, e1, e2) -> Let(v, r e1, r e2)
  | Unknown(s, t) -> Unknown(s, t)
  | Ite(e1, e2, e3) -> Ite(r e1, r e2, r e3)
  | Extract(i1, i2, e) -> Extract(i1, i2, r e)
  | Concat(e1, e2) -> Concat(r e1, r e2)
  in
  assert (e != newe);
  newe

let parse_extract = function
     | Cast(CAST_LOW, t, BinOp(RSHIFT, e', Int(i, t2))) ->
     	 (*
     	    Original: extract 0:bits(t)-1, and then shift left by i bits.
     	    New: extract i:bits(t)-1+i
     	 *)
     	 let et = infer_ast ~check:false e' in
     	 let bits_t = big_int_of_int (bits_of_width t) in
     	 let lbit = i in
     	 let hbit = (lbit +% bits_t) -% bi1 in
     	 (* XXX: This should be unsigned >, but I don't think it matters. *)
     	 if hbit >% big_int_of_int(bits_of_width et) then
     	   None
	 else
	   Some(hbit, lbit)
     | _ -> None

let parse_concat = function
    (* Note: We should only parse when we would preserve the type.
       So, (nt1=nt2) = bits(er) + bits(el)

       XXX: When we convert to normalized memory access, we get
       expressions like Cast(r32)(mem[0]) @ Cast(r32)(mem[1]) << 8 @
       ....  It sure would be nice if we could recognize this as a
       series of concats. *)
  | BinOp(OR,
	  BinOp(LSHIFT,
		Cast(CAST_UNSIGNED, nt1, el),
		Int(bits, _)),
	  Cast(CAST_UNSIGNED, nt2, er))
  | BinOp(OR,
	  Cast(CAST_UNSIGNED, nt2, er),
	  BinOp(LSHIFT,
		Cast(CAST_UNSIGNED, nt1, el),
		Int(bits, _)))
      when nt1 = nt2
	&& bits ==% big_int_of_int(bits_of_width (infer_ast ~check:false er))
	&& bits_of_width nt1 = bits_of_width (infer_ast ~check:false el) + bits_of_width (infer_ast ~check:false er) (* Preserve the type *)
	->
      Some(el, er)
  | BinOp(OR,
	  BinOp(LSHIFT,
		Cast(CAST_UNSIGNED, nt1, el),
		Int(bits, _)),
	  (Int(i, nt2) as er))
  | BinOp(OR,
	  (Int(i, nt2) as er),
	  BinOp(LSHIFT,
		Cast(CAST_UNSIGNED, nt1, el),
		Int(bits, _)))
      (* If we cast to nt1 and nt2 and we get the same thing, the
	 optimizer probably just dropped the cast. *)
      when Arithmetic.to_big_int (i, nt2) ==% Arithmetic.to_big_int (i, nt1)
	&& bits ==% big_int_of_int(bits_of_width (infer_ast ~check:false er))
	&& bits_of_width nt1 = bits_of_width (infer_ast ~check:false el) + bits_of_width (infer_ast ~check:false er) (* Preserve the type *)
	->
      Some(el, er)
  | _ -> None

(* Functions for removing expression types

   Should these recurse on subexpressions?
*)
let rm_ite = function
  | Ite(b, e1, e2) ->
      let t = Typecheck.infer_ast e1 in
      (match t with
      | Reg(1) ->
	(b &* e1) |*  (exp_not b &* e2)
      | Reg n ->
	((cast_signed t b) &* e1) |* ((cast_signed t (exp_not b)) &* e2)
      | _ -> failwith "rm_ite does not work with memories")
  | _ -> assert false (* Should we just act as a noop? *)

let rm_extract = function
  | Extract(h, l, e) ->
      let nb = int_of_big_int ((h -% l) +% bi1) in
      let nt = Reg(nb) in
      assert(h >=% bi0);
      assert (nb >= 0);
      let t = infer_ast ~check:false e in
      let e = if l <>% bi0 then e >>* Int(l, t) else e in
      let e = if t <> nt then cast_low nt e else e in
      e
  | _ -> assert false

let rm_concat = function
  | Concat(le, re) ->
      let bitsl,bitsr =
	Typecheck.bits_of_width (Typecheck.infer_ast ~check:false le),
	Typecheck.bits_of_width (Typecheck.infer_ast ~check:false re)
      in
      let nt = Reg(bitsl + bitsr) in
      exp_or ((cast_unsigned nt le) <<* Int(big_int_of_int bitsr, nt)) (cast_unsigned nt re)
  | _ -> assert false

let last_meaningful_stmt p =
  let rec f = function
    | Comment _::tl -> f tl
    | x::_ -> x
    | [] -> failwith "No meaningful statements"
  in
  f (List.rev p)

let reverse_bytes e =
  let bytes = Typecheck.bytes_of_width (Typecheck.infer_ast ~check:false e) in
  let get_byte n = extract (biconst (n*8+7)) (biconst (n*8)) e in
  reduce
    (fun bige e -> bige ++* e)
    (map get_byte (0 -- (bytes-1)))

(* Extract the nth least significant byte from e, starting with zero *)
let extract_byte n e =
  extract (biconst (n*8+7)) (biconst (n*8)) e

(* Concatenate a list of expressions *)
let concat_explist elist =
  reduce
    (fun l r -> l ++* r) elist
