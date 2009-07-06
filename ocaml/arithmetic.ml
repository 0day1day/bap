(** Basic integer arithmetic on N-bit integers (N < 64)

    These are common operations which are needed for constant folding or
    evaluation.

    @author Ivan Jager
 *)

open Type

let bits_of_width = Typecheck.bits_of_width

(* drop high bits *)
let to64 (i,t) =
  let bits = 64 - bits_of_width t in
  Int64.shift_right_logical (Int64.shift_left i bits) bits


(* sign extend to 64 bits*)
let tos64 (i,t) =
  let bits = 64 - bits_of_width t in
  Int64.shift_right (Int64.shift_left i bits) bits

  
(* shifting by more than the number of bits or by negative values
 * will be the same as shifting by the number of bits. *)
let toshift shiftedt v =
  let max = bits_of_width shiftedt
  and i = to64 v in
    if i <= Int64.of_int max && i >= 0L
    then Int64.to_int i
    else
      (prerr_endline("Warning: shifting "^string_of_int max^"-bit value by "
		    ^Int64.to_string i);
       max)

(* "cast" an int64 to a value *)
let to_val t i =
  let mask = Int64.shift_right_logical (-1L) (64-bits_of_width t) in
    (Int64.logand mask i, t)

let exp_bool =
  let t = (1L, Ast.reg_1)
  and f = (0L, Ast.reg_1) in
  (fun b -> if b then t else f)

(** [binop operand lhs lhst rhs rhst] *)
let binop op ((_,t) as v1) v2 =
  match op with
  | PLUS -> to_val t (Int64.add (to64 v1) (to64 v2))
  | MINUS -> to_val t (Int64.sub (to64 v1) (to64 v2))
  | TIMES -> to_val t (Int64.mul (to64 v1) (to64 v2))
  | AND -> to_val t (Int64.logand (to64 v1) (to64 v2))
  | OR -> to_val t (Int64.logor (to64 v1) (to64 v2))
  | XOR -> to_val t (Int64.logxor (to64 v1) (to64 v2))
  | EQ -> exp_bool((to64 v1) = (to64 v2))
  | NEQ -> exp_bool((to64 v1) <> (to64 v2))
  | LSHIFT -> to_val t (Int64.shift_left (to64 v1) (toshift t v2))
  | RSHIFT -> to_val t (Int64.shift_right_logical (to64 v1) (toshift t v2))
  | ARSHIFT -> to_val t (Int64.shift_right (tos64 v1) (toshift t v2))
  | DIVIDE -> to_val t (Util.int64_udiv (tos64 v1) (tos64 v2))
      (* Int64.div rounds towards zero. What do we want? *)
  | SDIVIDE -> to_val t (Int64.div (tos64 v1) (tos64  v2))
  | MOD -> to_val t (Int64.rem (tos64 v1) (tos64 v2))
  | SMOD -> to_val t (Int64.rem (tos64 v1) (tos64 v2))
  | SLT -> exp_bool(tos64 v1 < tos64 v2)
  | SLE -> exp_bool(tos64 v1 <= tos64 v2)
  | LT -> exp_bool(Util.int64_ucompare (to64 v1) (to64 v2) < 0)
  | LE -> exp_bool(Util.int64_ucompare (to64 v1) (to64 v2) <= 0)


let unop op ((_,t) as v) =
  match op with
  | NEG -> to_val t (Int64.neg (to64 v))
  | NOT -> to_val t (Int64.lognot (to64 v))


let cast ct ((_,t) as v) t2 =
  let bits1 = bits_of_width t
  and bits = bits_of_width t2 in
  (match ct with
   | CAST_UNSIGNED ->
       to_val t2 (to64  v)
   | CAST_SIGNED ->
       to_val t2 (tos64  v)
   | CAST_HIGH ->
       to_val t2
	 (Int64.shift_right 
	    (Int64.logand (to64  v)
	       (Int64.shift_left (-1L) (bits1-bits)) )
	    (bits1-bits) )
   | CAST_LOW ->
       to_val t2
	 (Int64.logand (to64  v)
	    ((Int64.lognot(Int64.shift_left (-1L) bits))) )
  )

