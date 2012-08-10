(** Copy propagation analysis. *)

(** Compute copy propagation for SSA CFGs. Returns a tuple [m,lookup]. m maps all variables with known copy propagations to their copy propagated expression.  [lookup e] will perform copy propagation on all variables in [e]. *)
val copyprop_ssa : Cfg.SSA.G.t -> Ssa.exp Var.VarMap.t * (Ssa.exp -> Ssa.exp)

(** Same as {!copyprop_ssa} but for AST CFGs. Note that analysis
    results for AST CFGs are not global, and so the analysis returns
    information before the specified program statement. *)
val copyprop_ast : Cfg.AST.G.t -> (Cfg.aststmtloc -> Ast.exp Var.VarMap.t * (Ast.exp -> Ast.exp))
