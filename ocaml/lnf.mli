(** Steensgard's loop nesting algorithm

    See Steensgaard, B. (1993). Sequentializing Program Dependence
    Graphs for Irreducible Programs (No. MSR-TR-93-14).
*)

(* body is a superset of headers. *)
type 'a lnt = { headers: 'a list; body: 'a list; children: 'a lnf }
and 'a lnf = 'a lnt list

val steensgard_ast : Cfg.AST.G.t -> Cfg.AST.G.V.t lnf
val steensgard_ssa : Cfg.SSA.G.t -> Cfg.SSA.G.V.t lnf
