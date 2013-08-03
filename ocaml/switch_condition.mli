(** Rewrite the outgoing edge conditions from (indirect jump) switches
    so they are in terms of the input variable, and not the target
    destination.

    @author ejs
 *)

(** Adds switch conditions to a SSA graph.  Takes the information
    returned by the VSA CFG recovery analysis as input. Returns a
    modified CFG when successful.  Success is currently defined to
    mean that edges from all indirect jumps can be rewritten (except
    those for returns). *)
val add_switch_conditions_disasm : Asmir_disasm.vsaresult -> Cfg.SSA.G.t option

(** Like [add_switch_conditions_disasm], but starts from an arbitrary
    SSA CFG.  Re-runs VSA analysis. *)
val add_switch_conditions_ssacfg : Asmir.asmprogram -> Cfg.SSA.G.t -> Cfg.SSA.G.t option
