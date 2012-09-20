(** Framework for incremental disassembly methods *)

(* XXX: Change interface to allow abstract interpretation. *)

open Type

type succs = | Addrs of label list
             | Error
             | Exit
             | Indirect

module type DISASM = sig
  type ret (** Auxiliary data that is returned after disassembly *)
  val initial_ret : ret
  val get_succs : Cfg.AST.G.t -> Cfg.AST.G.V.t * Cfg.AST.G.E.label * Ast.exp -> succs (** Function that returns the successors of a node *)
end

module Make :
  functor (D:DISASM) ->
sig
  val disasm_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * D.ret
  val disasm : Asmir.asmprogram -> Cfg.AST.G.t * D.ret
end

val recursive_descent : Asmir.asmprogram -> Cfg.AST.G.t * unit
val recursive_descent_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * unit
