(** Dataflow module for use with Control Flow Graphs.  This should
    probably always be used in BAP instead of [graphDataFlow]. *)

open GraphDataflow

(** Types of control flow graph that data flow is defined on *)
module type CFG =
sig
  type t
  type exp
  type stmt
  type lang = stmt list
  module V : Graph.Sig.COMPARABLE
  module E : Graph.Sig.EDGE with type vertex = V.t and type label = (bool * exp) option
  val pred_e : t -> V.t -> E.t list
  val succ_e : t -> V.t -> E.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val get_stmts : t -> V.t -> lang
end

(** A dataflow problem is defined by a lattice over a CFG. *)
module type DATAFLOW =
sig

  module L : BOUNDED_MEET_SEMILATTICE
  module G : CFG

  (** The transfer function over statements. *)
  val stmt_transfer_function : G.t -> G.stmt -> L.t -> L.t

  (** The transfer function over edge elements, e.g., conditions. *)
  val edge_transfer_function : G.t -> G.exp option -> L.t -> L.t

  (** The starting node for the analysis. *)
  val s0 : G.t -> G.V.t

  (** The initial lattice value given to node [s0]. All other nodes
      start out with [Top]. *)
  val init : G.t -> L.t

  (** The dataflow direction. *)
  val dir : direction
end

(** A dataflow problem with widening. *)
module type DATAFLOW_WITH_WIDENING =
sig

  module L : BOUNDED_MEET_SEMILATTICE_WITH_WIDENING
  module G : CFG

  (** The transfer function over statements. *)
  val stmt_transfer_function : G.t -> G.stmt -> L.t -> L.t

  (** The transfer function over edge elements, e.g., conditions. *)
  val edge_transfer_function : G.t -> G.exp option -> L.t -> L.t

  (** The starting node for the analysis. *)
  val s0 : G.t -> G.V.t

  (** The initial lattice value given to node [s0]. All other nodes
      start out with [Top]. *)
  val init : G.t -> L.t

  (** The dataflow direction. *)
  val dir : direction
end

(** Build a custom dataflow algorithm for the given dataflow problem [D]. *)
module Make :
  functor (D : DATAFLOW) ->
sig
  (** [worklist_iterate g] returns a worklist algorithm for graph [g]
      as a pair of functions [in,out]. [in], when given a node [v],
      computes the lattice value going in to that node, [v]. [out],
      when given a node [v], computes the lattice value exiting
      [v]. *)
  val worklist_iterate : ?init:(D.G.t -> D.L.t) ->
    D.G.t -> (D.G.V.t -> D.L.t) * (D.G.V.t -> D.L.t)

  (** Like [worklist_iterate], except the dataflow is done on the
      statement level. A statement [bb,n] is the [n]th stmt
      (zero-indexed) in [bb]. *)
  val worklist_iterate_stmt : ?init:(D.G.t -> D.L.t) ->
    D.G.t -> (D.G.V.t * int -> D.L.t) * (D.G.V.t * int -> D.L.t)
end

(** Build a custom dataflow algorithm for the given dataflow problem
    with widening operator [D]. *)
module MakeWide :
  functor (D : DATAFLOW_WITH_WIDENING) ->
sig
  (** Same as [worklist_iterate], but additionally employs the
      widening operator as lattice values propagate over backedges in
      the CFG.  Backedges are identified by observing when lattices
      values flow in cycles.

      @param nmeets The number of times a widening edge should be
      computed using meet.  After this threshold is reached, the widening
      operator will be applied.
  *)
  val worklist_iterate_widen : ?init:(D.G.t -> D.L.t) ->
    ?nmeets:int -> D.G.t -> (D.G.V.t -> D.L.t) * (D.G.V.t -> D.L.t)
end
