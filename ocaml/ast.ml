(** 
    The Abstract Syntax Tree.
    This IL allows nested expressions, making it closer to VEX and
    the concrete syntax than our SSA form. However, in most cases, this
    makes analysis harder, so you will generally want to convert to SSA
    for analysis.

    @author Ivan Jager
*)

open ExtList
open Type

type var = Var.t

type exp = 
  | Load of exp * exp * exp * typ  (** Load(arr,idx,endian, t) *)
  | Store of exp * exp * exp * exp * typ  (** Store(arr,idx,val, endian, t) *)
  | BinOp of binop_type * exp * exp
  | UnOp of unop_type * exp
  | Var of var
  | Lab of string
  | Int of int64 * typ
  | Cast of cast_type * typ * exp (** Cast to a new type. *)
  | Let of var * exp * exp
  | Unknown of string * typ

type attrs = Type.attributes

type stmt =
  | Move of var * exp * attrs  (** Assign the value on the right to the
				      var on the left *)
  | Jmp of exp * attrs (** Jump to a label/address *)
  | CJmp of exp * exp * exp * attrs
      (** Conditional jump. If e1 is true, jumps to e2, otherwise jumps to e3 *)
  | Label of label * attrs (** A label we can jump to *)
  | Halt of exp * attrs
  | Assert of exp * attrs
  | Comment of string * attrs (** A comment to be ignored *)
  | Special of string * attrs (** A "special" statement. (does magic) *)

type program = stmt list

(** Make an expression corresponding to the given label, for use as the
    target of a [Jmp]. *)
let exp_of_lab = function
  | Name s -> Lab s
  | Addr a -> Int(a, Reg 64)

(** If possible, make a label that would be refered to by the given
    expression. *)
let lab_of_exp = function
  | Lab s -> Some(Name s)
  | Int(i, Reg bits) ->
      Some(Addr(Int64.logand i (Int64.pred(Int64.shift_left 1L bits))))
  | _ -> None
    

let reg_1 = Reg 1
and reg_8 = Reg 8
and reg_16 = Reg 16
and reg_32 = Reg 32
and reg_64 = Reg 64

(** False constant. (If convenient, refer to this rather than building your own.) *)
let exp_false = Int(0L, reg_1)
(** True constant. *)
let exp_true = Int(1L, reg_1)

(** More convenience functions for building common expressions. *)
let exp_and e1 e2 = BinOp(AND, e1, e2)
let exp_or e1 e2 = BinOp(OR, e1, e2)
let exp_eq e1 e2 = BinOp(EQ, e1, e2)
let exp_not e = UnOp(NOT, e)
let exp_implies e1 e2 = exp_or (exp_not e1) e2

