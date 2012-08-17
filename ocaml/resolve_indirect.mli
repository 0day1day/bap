(** Resolve indirect jumps using Value Set Analysis *)

(** [resolve_indjumps asmp cfg] will resolve the possible targets of
    indirect jumps in [cfg], and refine [cfg] to reflect the possible
    targets. Any node [v] where [is_exit v] returns [true] is
    considered an exit node. *)
val resolve_indjumps : ?is_exit:(Cfg.AST.G.V.t -> bool) -> Asmir.asmprogram -> Cfg.AST.G.t -> Cfg.AST.G.t
