val explode_mem_let : (Ast.exp -> bool) -> Ast.exp -> Ast.exp
val flatten_memexp_rev :
  Ast.var -> Ast.attrs -> Ast.exp -> Ast.exp * Ast.stmt list
val flatten_memexp :
  Ast.var -> Ast.attrs -> Ast.exp -> Ast.exp * Ast.stmt list
val flatten_stores : Ast.stmt -> Ast.stmt list
val flatten_loads : Ast.exp -> Ast.exp
