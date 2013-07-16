(** Simplify predicates so that VSA and other abstract interpretations
    can use them. *)

(** Simplify conditions used in edge labels *)
val simplifycond_ssa : Cfg.SSA.G.t -> Cfg.SSA.G.t
