(** Framework for incremental disassembly methods *)

(* XXX: Change interface to allow abstract interpretation. *)

open Type

type succs = | Addrs of label list
             | Error
             | Exit
             | Indirect

module type DISASM = sig
  module State : sig
    type t (** Auxiliary data that is returned after disassembly *)
    val init : t
  end
  (* val visit_new_address : addr * string * Cfg.AST.G.V.t list * Cfg.AST.G.E.t list * Cfg.AST.G.V.t option -> State.t -> State.t *)
  (* visit_new_edge? *)
  val get_succs : Asmir.asmprogram -> Cfg.AST.G.t -> Cfg.AST.G.V.t * Cfg.AST.G.E.label * Ast.exp -> State.t -> succs * State.t (** Function that returns the successors of a node *)
end

module Make :
  functor (D:DISASM) ->
sig
  val disasm_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * D.State.t
  val disasm : Asmir.asmprogram -> Cfg.AST.G.t * D.State.t
end

val recursive_descent : Asmir.asmprogram -> Cfg.AST.G.t * unit
val recursive_descent_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * unit
val vsa : Asmir.asmprogram -> Cfg.AST.G.t * unit
val vsa_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * unit
