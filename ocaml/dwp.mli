(** An alternate formulation of DWP that is easier to understand,
    generates smaller formulas for programs without [Assume]
    statements.

    @author ejs
*)

val eddwp :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [eddwp mode p q] is the same as {!dwp}, but uses an alternate
    formulation of dwp.  It is arguably easier to understand, and
    generates smaller formulas for programs that do not have [Assume]
    statements. *)

val eddwp_conc :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [eddwp_conc] is like {!eddwp} but with concrete evaluation turned
    on. *)
