(** Control Dependence Graphs. The PDG works on SSA stmt lists.  If
    the stmt list contains more than a single statment, you may get
    more edges than strictly necessary.  Use Ssa.ssalist_to_ssa to
    break an ssa list cfg into an ssa list cfg where each list has only
    one statement.
*)

open Cfg

(** Control dependence graph builder. *)
module Make(C: CFG) = 
struct
  module G = C.G

  module Dbg = Debug.Make(struct let name = "CDG" and default=`NoDebug end)
  open Dbg

  (* the graph we will return *)
  module CDG = Graph.Persistent.Digraph.Concrete(G.V)
  include CDG

  (* reverse graph  *)
  module G' = struct
    type t = G.t
    module V = G.V

    let pred = G.succ 
    let succ = G.pred
    let nb_vertex = G.nb_vertex
    let fold_vertex = G.fold_vertex
    let iter_vertex = G.iter_vertex

  end 

  (* inverse dominators module *)
  module D = Dominator.Make(G') 
    

    
  (** This function builds a CDG (Control-Dependence Graph) for the given
      CFG. A CDG has an edge from x to y any time y is
      control-dependent on x.
      This implements the algorithm in the Tiger Book p.454 (ML
      version) with the exception we do not add a new node before
      entry. Thus, all nodes are control dependent on BB_Entry.
      
  *)
  let compute_cdg cfg =
    (* Note that we don't add an extra entry node, so everything is control
       dependent on the entry node of the CFG *)
    let cfg' = C.copy cfg in 
    let entry_node = G.V.create BB_Entry in 
    let exit_node = G.V.create BB_Exit in 
    let cfg' = if G.mem_edge cfg' entry_node exit_node then cfg' 
               else C.add_edge cfg' entry_node exit_node in 
    let () = dprintf "compute_cdg: computing idom" in
    let idom = D.compute_idom cfg' exit_node in
    let () = dprintf "compute_cdg: computing dom tree" in
    let dom_tree = D.idom_to_dom_tree cfg' idom in
    let () = dprintf "compute_cdg: computing dom frontier" in
    let df = D.compute_dom_frontier cfg' dom_tree idom in
    let vertices =  G.fold_vertex (fun v g -> add_vertex g v) cfg empty in
      G.fold_vertex
	(fun v g ->
	   if G.in_degree cfg v > 0
	   then List.fold_left (fun g v' -> add_edge g v' v) g (df v)
	   else g (* can't compute DF for lonely nodes *)
	)
	cfg' vertices
end

(** control dependence graphs for SSA graphs *)
module CDG_SSA = Make(Cfg.SSA)

(** control dependence graphs for AST graphs *)
module CDG_AST = Make(Cfg.AST)
