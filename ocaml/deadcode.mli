val do_dce : ?globals:Var.t list -> Cfg.SSA.G.t -> Cfg.SSA.G.t * bool
val cfg_jumpelim : Cfg.SSA.G.t -> Cfg.SSA.G.t
