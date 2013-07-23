(** Simplify predicates so that VSA and other abstract interpretations
    can use them. *)

(** Simplify conditions used in edge labels *)
val simplifycond_ssa : Cfg.SSA.G.t -> Cfg.SSA.G.t

(** Simplify conditions used in edge labels. This version is for
    resolving the target expression passed as an argument.  It does so by
    never copy propagating beyond one of the variables used in the target
    expression.  This should ensure that the simplified conditions are "in
    terms of" variables in the target expression.  This is important for
    VSA cfg recovery. *)
val simplifycond_target_ssa : Ssa.exp -> Cfg.SSA.G.t -> Cfg.SSA.G.t
