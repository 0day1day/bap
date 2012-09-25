(** Framework for incremental disassembly methods *)

(* XXX: Change interface to allow abstract interpretation. *)

open Type

type succs = | Addrs of label list
             | Error
             | Exit
             | Indirect

module type STATE = sig
  type t
  val init : t
end

module type FUNCID = sig
  module State : STATE
  val find_calls : Cfg.AST.G.t -> Cfg.AST.G.V.t list -> Cfg_ast.unresolved_edge list -> State.t -> Cfg_ast.unresolved_edge list * State.t
  val find_rets : Cfg.AST.G.t -> Cfg.AST.G.V.t list -> Cfg_ast.unresolved_edge list -> State.t -> Cfg_ast.unresolved_edge list * State.t
end

module type DISASM = sig
  module State : STATE
  val get_succs : Asmir.asmprogram -> Cfg.AST.G.t -> Cfg.AST.G.V.t * Cfg.AST.G.E.label * Ast.exp -> State.t -> succs * State.t (** Function that returns the successors of a node *)
end

module Make :
  functor (D:DISASM) ->
    functor (F:FUNCID) ->
sig
  val disasm_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * D.State.t
  val disasm : Asmir.asmprogram -> Cfg.AST.G.t * D.State.t
end

val recursive_descent : Asmir.asmprogram -> Cfg.AST.G.t * unit
val recursive_descent_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * unit
val vsa : Asmir.asmprogram -> Cfg.AST.G.t * unit
val vsa_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * unit
