(** Break complicated memory write statements a series of flat ones of
    form [Store(Var v, ...)]. This makes it easier to execute the
    memory operations sequentially.

    $Id$

    @author ejs
*)

(* val explode_mem_let : (Ast.exp -> bool) -> Ast.exp -> Ast.exp *)
(* val flatten_memexp_rev : *)
(*   Ast.var -> Ast.attrs -> Ast.exp -> Ast.exp * Ast.stmt list *)

(** [flatten_memexp memvl atts e] returns a tuple [(flate, stmts)]
    where [flate] is equivalent to [e] but flat (contains no nested
    Stores), as long as [stmts] are executed immediately before evaluating
    [flate]. *)
val flatten_memexp :
  Ast.var -> Ast.attrs -> Ast.exp -> Ast.exp * Ast.stmt list

(** Converts a deep assignment to memory [Move(memv, Store(Store(memv,
    idx1, value1), idx2, value2))] to multiple flat assignments:
    [Move(memv, Store(Var memv, idx1, value1)) :: Move(memv, Store(Var
    memv, idx2, value2)) :: []]. Statements that are not memory writes
    are passed through unchanged.  Lets with memory types are exploded.
*)
val flatten_stores : Ast.stmt -> Ast.stmt list

(** Converts a Load expression e into a form that can be sequentially
    evaluated concretely.  This means that there are no Let bindings that
    bind a memory state to a variable, because that cannot be implemented
    concretely. *)
val flatten_loads : Ast.exp -> Ast.exp

(** Flattens all memory loads and stores in a program. *)
val flatten_mem_program : Ast.program -> Ast.program
