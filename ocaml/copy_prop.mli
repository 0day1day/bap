(** Copy propagation *)

(** Compute copy propagation for SSA CFGs *)
val copyprop_ssa : Cfg.SSA.G.t -> Ssa.exp Var.VarMap.t

(** Compute copy propagation for AST CFGs *)
(* val copyprop_ast : Cfg.AST.G.t -> Ast.exp Var.VarMap.t *)
