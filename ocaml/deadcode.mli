(** [def_uses s] returns (defined list, used list, liveout
    list). liveout is statements marked as Type.liveout *)
val def_uses : Ssa.stmt  -> Ssa.var list  * Ssa.var list * Ssa.var list

val do_dce : ?globals:Var.t list -> Cfg.SSA.G.t -> Cfg.SSA.G.t * bool
