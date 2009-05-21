open Ast
type t =
  | Assume of exp
  | Assign of Var.t * exp
  | Assert of exp
  | Choice of t * t
  | Seq of t * t
  | Skip

(*val of_straightline : ?acc:t -> Ast.stmt list -> t*)
val of_astcfg : ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> t
val remove_skips : t -> t
