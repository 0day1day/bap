(** Utility functions for ASTs.  It's useful to have these in a
    separate file so it can use functions from Typecheck and elsewhere. *)

open Ast
open Type
open Typecheck

(* exp helpers *)
let binop op a b = match (a,b) with
  | (Int(a, at), Int(b, bt)) when at = bt ->
    let (i,t) = Arithmetic.binop op (a,at) (b,bt) in
    Int(i,t)
  | _ -> BinOp(op, a, b)

let (+*) a b   = binop PLUS a b
let (-*) a b   = binop MINUS a b
let ( ** ) a b   = binop TIMES a b
let (<<*) a b  = binop LSHIFT a b
let (>>*) a b  = binop RSHIFT a b
let (>>>*) a b = binop ARSHIFT a b
let (&*) a b   = binop AND a b
let (|*) a b   = binop OR a b
let (^*) a b   = binop XOR a b
let (=*) a b   = binop EQ a b
let (<*) a b   = binop LT a b
let (>*) a b   = binop LT b a

let cast_low t e = Cast(CAST_LOW, t, e)
let cast_high t e = Cast(CAST_HIGH, t, e)
let cast_signed t e = Cast(CAST_SIGNED, t, e)
let cast_unsigned t = function
  | Cast(CAST_UNSIGNED, Reg t', e) when Arithmetic.bits_of_width t >= t' ->
    Cast(CAST_UNSIGNED, t, e)
  | e ->
    Cast(CAST_UNSIGNED, t, e)

let exp_ite ?t b e1 e2 =
  (* FIXME: were we going to add a native if-then-else thing? *)
  (* type inference shouldn't be needed when t is specified, but we're paranoid *)
  let tb = Typecheck.infer_ast ~check:false b in
  let t1 = Typecheck.infer_ast ~check:false e1 in
  let t2 = Typecheck.infer_ast ~check:false e2 in
  assert (t1 = t2);
  assert (tb = Reg(1));

  (match t with
    | None -> ()
    | Some t -> assert (t=t1));

  Ite(b, e1, e2)
  

let parse_ite = function
  | BinOp(OR,
	  BinOp(AND, Cast(CAST_SIGNED, _, b1), e1),
	  BinOp(AND, Cast(CAST_SIGNED, _, UnOp(NOT, b2)), e2)
  ) 
  | BinOp(OR,
	  BinOp(AND, b1, e1),
	  BinOp(AND, UnOp(NOT, b2), e2)
  ) when b1 = b2 && Typecheck.infer_ast ~check:false b1 = Reg(1) -> 
    Some(b1, e1, e2)
      (* In case one branch is optimized away *)
  | BinOp(AND,
	  Cast(CAST_SIGNED, nt, b1),
	  e1) when Typecheck.infer_ast ~check:false b1 = Reg(1) ->
    Some(b1, e1, Int(0L, nt))
  | _ -> None

let parse_extract = function
     | Cast(CAST_LOW, t, BinOp(RSHIFT, e', Int(i, t2))) ->
     	 (* 
     	    Original: extract 0:bits(t)-1, and then shift left by i bits.
     	    New: extract i:bits(t)-1+i
     	 *)
     	 let et = infer_ast ~check:false e' in
     	 let bits_t = Int64.of_int (bits_of_width t) in
     	 let lbit = i in
     	 let hbit = Int64.pred (Int64.add lbit bits_t) in
     	 (* XXX: This should be unsigned >, but I don't think it matters. *)
     	 if hbit > Int64.of_int(bits_of_width et) then
     	   None
	 else
	   Some(hbit, lbit)
     | _ -> None

let parse_concat = function
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
      when nt1 = nt2 && bits = Int64.of_int(bits_of_width (infer_ast ~check:false er)) ->
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
      optimizer probably just dropped the case. *)
      when Arithmetic.to64 (i, nt2) = Arithmetic.to64 (i, nt1) ->
      Some(el, er)
  | _ -> None
	 
(* Functions for removing expression types 

   Should these recurse on subexpressions?
*)
let rm_ite = function
  | Ite(b, e1, e2) ->
      let t = Typecheck.infer_ast b in
      if t = reg_1 then
	(b &* e1) |*  (exp_not b &* e2)
      else
	((cast_signed t b) &* e1) |* ((cast_signed t (exp_not b)) &* e2) 
  | _ -> assert false (* Should we just act as a noop? *)

let rm_extract = function
  | Extract(h, l, e) ->
      let nb = Int64.to_int (Int64.succ (Int64.sub h l)) in
      let nt = Reg(nb) in
      assert(h >= 0L);
      assert (nb >= 0);
      let t = infer_ast ~check:false e in
      let e = if l <> 0L then e >>* Int(l, t) else e in
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
      exp_or ((cast_unsigned nt le) <<* Int(Int64.of_int bitsr, nt)) (cast_unsigned nt re)
  | _ -> assert false
