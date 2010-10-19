type result = Valid | Invalid | StpError | Timeout


val result_to_string : result -> string

val runstp : ?timeout:int -> string -> result

val writeformula :
  ?exists:Ast.var list ->  ?foralls:Ast.var list -> ?remove:bool
  -> Cfg.AST.G.t -> string

(* possibly expose write_formula too if that is useful *)

val query_formula :
  ?timeout:int -> ?exists:Ast.var list ->  ?foralls:Ast.var list
  -> Ast.exp -> result
