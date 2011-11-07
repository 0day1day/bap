open Ast
open Type

type callback = addr -> addr -> stmt list -> bool

val rdisasm_at : ?f:callback -> Asmir.asmprogram -> int64 -> Ast.program * string
val rdisasm : ?f:callback -> Asmir.asmprogram -> Ast.program * string
val max_callback : int -> callback
