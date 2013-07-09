(** Copy propagation analysis. *)

(** Compute copy propagation for SSA CFGs. Returns a tuple
    [m,lookup]. m maps all variables with known copy propagations to
    their copy propagated expression.  [lookup e] will perform copy
    propagation on all variables in [e]. Copy propagation will not
    propagate any lattice values over a statement [s] if the optional
    funtion [stop_at] is specified and [stop_at s = false]. *)
val copyprop_ssa : ?stop_at:(Ssa.stmt -> bool) -> Cfg.SSA.G.t -> Ssa.exp Var.VarMap.t * (Ssa.exp -> Ssa.exp)

(** Same as {!copyprop_ssa} but for AST CFGs. Note that analysis
    results for AST CFGs are not global, and so the analysis returns
    information before the specified program statement. The last tuple
    returned is a map from variables to their constant expression, if
    one exists, and the location of the assignment. *)
val copyprop_ast : ?stop_at:(Ast.stmt -> bool) -> Cfg.AST.G.t -> (Cfg.aststmtloc -> Ast.exp Var.VarMap.t * (Ast.exp -> Ast.exp) * (Cfg.aststmtloc * Ast.exp) Var.VarMap.t)
