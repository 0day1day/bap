open Ast
type t =
  | Assume of exp
  | Assign of Var.t * exp
  | Assert of exp
  | Choice of t * t
  | Seq of t * t
  | Skip

(** Intermediate form of gcl, can be useful for converting to non-GCL
    structured languages. *)
type gclhelp =
  | CAssign of Cfg.AST.G.V.t
  | CChoice of exp * gclhelp * gclhelp (* bb with cjmp, true  and false branches *)
  | Cunchoice of gclhelp * gclhelp (* unfinished choice *)
  | CSeq of gclhelp list

val gclhelp_of_astcfg : ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> gclhelp

val of_astcfg : ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> t
val of_ast : Ast.program -> t

(* val remove_skips : t -> t *)

val passified_of_ssa :
  ?entry:Cfg.SSA.G.V.t -> ?exit:Cfg.SSA.G.V.t -> Cfg.SSA.G.t -> t * var list
val passified_of_astcfg :
  ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> t * var list * (Var.t->Var.t)
