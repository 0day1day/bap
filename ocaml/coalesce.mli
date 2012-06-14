(** Coalesce sequential basic blocks into a single basic block.

    A sequence [seq] of basic blocks [bb1, ..., bbn] is sequential if
    - [seq] is the only path from [bb1] to [bbn] in the control flow graph.
    - [bb1] dominates all other basic blocks in [seq].

    XXX: Update this definition to reflect noop BBs.

*)

val coalesce_ast : Cfg.AST.G.t -> Cfg.AST.G.t
(** coalesce_ast [cfg] returns a new AST CFG in which sequential basic
    blocks in [cfg] are coalesced into a single basic block. *)
val coalesce_ssa : Cfg.SSA.G.t -> Cfg.SSA.G.t
(** coalesce_ssa [cfg] returns a new SSA CFG in which sequential basic
    blocks in [cfg] are coalesced into a single basic block. *)
