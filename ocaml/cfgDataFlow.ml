(* Dataflow for CFGs

   XXX: MakeWide should be the main functor

*)

open GraphDataflow

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

module type DATAFLOW =
sig

  module L : BOUNDED_MEET_SEMILATTICE
  module G : CFG

  val stmt_transfer_function : G.t -> G.stmt -> L.t -> L.t

  val edge_transfer_function : G.t -> G.exp option -> L.t -> L.t

  val s0 : G.t -> G.V.t

  val init : G.t -> L.t

  val dir : direction
end

module type DATAFLOW_WITH_WIDENING =
sig

  module L : BOUNDED_MEET_SEMILATTICE_WITH_WIDENING
  module G : CFG

  val stmt_transfer_function : G.t -> G.stmt -> L.t -> L.t

  val edge_transfer_function : G.t -> G.exp option -> L.t -> L.t

  val s0 : G.t -> G.V.t

  val init : G.t -> L.t

  val dir : direction
end

module Make (D:DATAFLOW) =
struct
  let fold f l stmts = match D.dir with
    | Forward -> List.fold_left (fun a b -> f b a) l stmts
    | Backward -> BatList.fold_right f stmts l
  module DFSPEC =
  struct
    module L=D.L
    module G=D.G
    let node_transfer_function g v l =
      fold (D.stmt_transfer_function g) l (G.get_stmts g v)
    let edge_transfer_function g e l =
      let arg = match G.E.label e with
        | Some(_,e) -> Some e
        | None -> None
      in
      D.edge_transfer_function g arg l
    let s0 = D.s0
    let init = D.init
    let dir = D.dir
  end
  module DF = GraphDataflow.Make(DFSPEC)
  let worklist_iterate =
    DF.worklist_iterate
  let worklist_iterate_stmt ?init g =
    let win,wout = worklist_iterate ?init g in
    let winstmt (v,n) =
      let l = win v in
      fold (D.stmt_transfer_function g) l (BatList.take n (D.G.get_stmts g v))
    and woutstmt (v,n) =
      let l = win v in
      fold (D.stmt_transfer_function g) l (BatList.take (n+1) (D.G.get_stmts g v))
    in
    winstmt, woutstmt
end

module MakeWide (D:DATAFLOW_WITH_WIDENING) =
struct
  include Make(D)
  let fold f l stmts = match D.dir with
    | Forward -> List.fold_left (fun a b -> f b a) l stmts
    | Backward -> BatList.fold_right f stmts l
  module DFSPECW = struct
    module L=D.L
    module G=D.G
    let node_transfer_function g v l =
      fold (D.stmt_transfer_function g) l (G.get_stmts g v)
    let edge_transfer_function g e l =
      let arg = match G.E.label e with
        | Some(_,e) -> Some e
        | None -> None
      in
      D.edge_transfer_function g arg l
    let s0 = D.s0
    let init = D.init
    let dir = D.dir
  end
  module DFW = GraphDataflow.MakeWide(DFSPECW)
  let worklist_iterate_widen =
    DFW.worklist_iterate_widen
  let worklist_iterate_stmt ?init g =
    let win,wout = worklist_iterate_widen ?init g in
    let winstmt (v,n) =
      let l = win v in
      fold (D.stmt_transfer_function g) l (BatList.take n (D.G.get_stmts g v))
    and woutstmt (v,n) =
      let l = win v in
      fold (D.stmt_transfer_function g) l (BatList.take (n+1) (D.G.get_stmts g v))
    in
    winstmt, woutstmt
end
