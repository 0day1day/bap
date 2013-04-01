(** Steensgard's loop nesting algorithm

    See Steensgaard, B. (1993). Sequentializing Program Dependence
    Graphs for Irreducible Programs (No. MSR-TR-93-14).
*)

module type G =
sig
  include Graph.Sig.I

  val remove_edge_e : t -> E.t -> unit
  val v2s : V.t -> string
  val nb_vertex : t -> int
end

(* body is a superset of headers. *)
(* headers, body, and children are all sorted *)
type 'a lnt = { headers: 'a list; body: 'a list; children: 'a lnf }
and 'a lnf = 'a lnt list

val validate_lnf : 'a lnf -> bool
val validate_lnt : 'a lnt -> bool

val string_of_lnf : ('a -> string) -> 'a lnf -> string
val string_of_lnt : ('a -> string) -> 'a lnt -> string

module type MakeType =
  functor (Gr: G) ->
    sig
      val lnf : Gr.t -> Gr.V.t -> Gr.V.t lnf
    end

module Dot : functor(Gr: G) ->
sig
  val to_dot: ?e2s:(Gr.E.t -> string) -> Gr.t -> Gr.V.t lnf -> string
end
