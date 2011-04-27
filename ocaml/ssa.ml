(** 
    Static Single Assignment form.
    This is the intermediate language where most analysis should be happening.

    @author Ivan Jager
*)

open Big_int
open ExtList
open Type

type var = Var.t

type value =
  | Int of big_int * typ
  | Var of var
  | Lab of string


type exp = 
  | Load of value * value * value * typ  (** Load(arr,idx,endian, t) *)
  | Store of value * value * value * value * typ  (** Store(arr,idx,val, endian, t) *)
  | BinOp of binop_type * value * value
  | UnOp of unop_type * value
  | Val of value
  | Cast of cast_type * typ * value (** Cast to a new type. *)
  | Unknown of string * typ
  | Phi of var list
      (** Joins variables that were assigned over different paths *)

type attrs = Type.attributes

type stmt =
  | Move of var * exp * attrs  (** Assign the value on the right to the
				      var on the left *)
  | Jmp of value * attrs (** Jump to a label/address *)
  | CJmp of value * value * value * attrs
      (** Conditional jump. If e1 is true, jumps to e2, otherwise jumps to e3 *)
  | Label of label * attrs (** A label we can jump to *)
  | Halt of value * attrs
  | Assert of value * attrs
  | Comment of string * attrs (** A comment to be ignored *)
  (* | Special of string * attrs (** A "special" statement. (does magic) *) *)

let val_false = Int(zero_big_int, Ast.reg_1)
let val_true = Int(unit_big_int, Ast.reg_1)

(** If possible, make a label that would be refered to by the given
    expression. *)
let val_of_exp = function
  | Lab s -> Some(Name s)
  | Int(i, t) ->
      Some(Addr(int64_of_big_int (Arithmetic.to64 (i,t))))
  | _ -> None
