val v2s : Cfg.AST.G.V.t -> string

val of_prog : ?special_error:bool -> Ast.program -> Cfg.AST.G.t

val to_prog : Cfg.AST.G.t -> Ast.program
