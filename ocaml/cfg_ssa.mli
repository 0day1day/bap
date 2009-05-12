(** 
    Static Single Assignment translation

    @todo Export trans_cfg, after finding a good name for it.

    @author Ivan Jager
*)


val of_astcfg : Cfg.AST.G.t -> Cfg.SSA.G.t

val of_ast : Ast.program -> Cfg.SSA.G.t

val to_astcfg : ?remove_temps:bool -> Cfg.SSA.G.t -> Cfg.AST.G.t

val to_ast : ?remove_temps:bool -> Cfg.SSA.G.t -> Ast.program
