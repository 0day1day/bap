(** An alternate formulation of DWP that is easier to understand,
    generates smaller formulas for programs without [Assume]
    statements.

    @author ejs
*)

val eddwp :
  ?normalusage:bool ->
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [eddwp mode p q] is the same as {!dwp}, but uses an alternate
    formulation of dwp.  It is arguably easier to understand, and
    generates smaller formulas for programs that do not have [Assume]
    statements. *)

val eddwp_uwp :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> Type.formula_mode -> Gcl.Ugcl.t -> Ast.exp -> Ast.exp
(** [eddwp_uwp] is a version of [eddwp] that operates directly on a
    CFG. *)

val eddwp_lazyconc :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> ?cf:bool -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [eddwp_lazyconc] is like {!eddwp} but with concrete evaluation and
    lazy merging turned on. *)

val eddwp_lazyconc_uwp :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> ?cf:bool -> Type.formula_mode -> Gcl.Ugcl.t -> Ast.exp -> Ast.exp
(** [eddwp_lazyconc_uwp] is a version of [eddwp_lazyconc] that
    operates directly on a CFG. *)
