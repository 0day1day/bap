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
  val get_succs : Asmir.asmprogram -> Cfg.AST.G.t -> Cfg_ast.unresolved_edge list -> State.t -> (Cfg_ast.unresolved_edge * succs) list * State.t
  (** Function that returns the successors of one or more nodes in the
      unresolved list. *)

  val fixpoint : bool
  (** Should [get_succs] be called until a fixpoint is reached? *)

end

module Make :
  functor (D:DISASM) ->
    functor (F:FUNCID) ->
sig
  val disasm_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t * D.State.t
  val disasm : Asmir.asmprogram -> Cfg.AST.G.t * D.State.t
end

val recursive_descent : Asmir.asmprogram -> Cfg.AST.G.t
val recursive_descent_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t

type vsaresult = {origssa: Cfg.SSA.G.t;
                  optssa: Cfg.SSA.G.t;
                  vsa_in: Cfg.ssastmtloc -> Vsa_ssa.AbsEnv.t option;
                  vsa_out: Cfg.ssastmtloc -> Vsa_ssa.AbsEnv.t option;}

val vsa_full : Asmir.asmprogram -> Cfg.AST.G.t * vsaresult option
val vsa_at_full : Asmir.asmprogram -> addr -> Cfg.AST.G.t * vsaresult option


val vsa : Asmir.asmprogram -> Cfg.AST.G.t
val vsa_at : Asmir.asmprogram -> addr -> Cfg.AST.G.t
