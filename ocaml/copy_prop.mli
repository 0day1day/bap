(** Copy propagation *)

(** Compute copy propagation for SSA CFGs *)
val copyprop_ssa : Cfg.SSA.G.t -> Ssa.exp Var.VarMap.t
