(** Dependence Graphs. We currently support the program dependence
    graph (PDG), a data dependence graph (DDG), and the control
    dependence graph (CDG). 
*)

open Cfg

module MakeCDG (C: CFG) = 
struct
  module G = C.G

  module Dbg = Debug.Make(struct let name = "CDG" and default=`NoDebug end)
  open Dbg

  (* the graph we will return *)
(*  module CDG =   Graph.Persistent.Digraph.Concrete(G.V) *)
(*  include CDG *)

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
    

    
  (** This function computes control dependencies.
      This implements the algorithm in the Tiger Book p.454 (ML
      version) with the exception we do not add a new node before
      entry. Therefore all nodes are control dependent on BB_Entry.
      
      Note that BB_Exit will not be control dependent on anything,
      thus a lone node in the graph (you can prune it away if you want
      using other utilities in BAP)

      @return a map from a node to its parents in the CDG tree.

  *)
  let compute_cd cfg =
    (* Note that we don't add an extra entry node, so everything is control
       dependent on the entry node of the CFG *)
    let cfg' = C.copy cfg in 
    let entry_node = C.find_vertex cfg' BB_Entry in 
    let exit_node = C.find_vertex cfg' BB_Exit in 
    let cfg' = if G.mem_edge cfg' entry_node exit_node then cfg' 
               else C.add_edge cfg' entry_node exit_node in 
    let () = dprintf "compute_cdg: computing idom" in
    let idom = D.compute_idom cfg' exit_node in
    let () = dprintf "compute_cdg: computing dom tree" in
    let dom_tree = D.idom_to_dom_tree cfg' idom in
    let () = dprintf "compute_cdg: computing dom frontier" in
      D.compute_dom_frontier cfg' dom_tree idom 

(** computes the control dependence graph (cdg), which turns the
    result of [compute_cd] below into a graph*)
  let compute_cdg cfg  = 
    let df = compute_cd cfg in 
    let vertices =  C.G.fold_vertex (fun v g -> C.add_vertex g v) cfg (C.empty ()) in
      C.G.fold_vertex
	(fun v g ->
	   if C.G.in_degree cfg v > 0
	   then List.fold_left (fun g v' -> C.add_edge g v' v) g (df v)
	   else g (* can't compute DF for lonely nodes *)
	)
	cfg vertices

end

(** control dependence graphs for SSA graphs *)
module CDG_SSA = MakeCDG(Cfg.SSA)

(** control dependence graphs for AST graphs *)
module CDG_AST = MakeCDG(Cfg.AST)


module DDG_SSA = 
struct

  (** a location in the CFG program.  *)
  type location = Cfg.SSA.G.V.t * int

  module Dbg = Debug.Make(struct let name = "DDG" and default=`NoDebug end)
  open Dbg
  module VH = Var.VarHash
  module VS = Var.VarSet

  (** [compute_dd cfg] the tuple vars,fd,fu. vars is the set of
      variables used by the graph.  fd is a hashtbl from vars to the
      definition location.  fu is a hashtbl from vars to their use
      locations.  Unlike graphs such as DDG and PDG (below), we do not
      assume that vars are defined on entry and used on exit.  *)
  let compute_dd cfg = 
    let defs:(location) VH.t = VH.create 65535 in 
    let uses = VH.create 65535 in 
    let vars = ref VS.empty in 
    let update_def_uses s (loc:SSA.G.V.t * int) =
      let () = 
	match s with
	  | Ssa.Move (lv, _, _) -> VH.add defs lv loc
	  | _ -> ()
      in
      let stmt_uses = ref VS.empty in 
      let vis =  object(self)
	inherit Ssa_visitor.nop
	method visit_rvar v = stmt_uses := VS.add v !stmt_uses; `DoChildren
	method visit_value v = 
	  match v with
	      Ssa.Var(v') -> vars := VS.add v' !vars; `DoChildren
	    | _ -> `DoChildren
      end
      in
	ignore (Ssa_visitor.stmt_accept vis s);
	VS.iter (fun v -> 
		   let prev = try VH.find uses v with Not_found -> [] in 
		     VH.replace uses v (loc::prev)
		) !stmt_uses
    in
      SSA.G.iter_vertex 
	(fun v ->
	   ignore(List.fold_left 
		    (fun idx s -> update_def_uses s (v,idx); idx+1)
		    0 (SSA.get_stmts cfg v)
		 )
	) cfg;
      (!vars,defs,uses)

  let compute_ddg cfg = 
    let vars,defs,uses = compute_dd cfg in 
      (* the following 2 statements copy only the cfg verticies  *)
    let ddg = SSA.copy cfg in 
    let ddg = SSA.G.fold_edges 
	(fun src dst cfg' -> 
	    let s = SSA.find_vertex cfg' (SSA.G.V.label src) in 
	    let d = SSA.find_vertex cfg' (SSA.G.V.label dst) in 
	      SSA.remove_edge cfg' s d
	) cfg ddg
    in
    let entryv = SSA.find_vertex ddg BB_Entry in 
      (* fd and fu implement assumption all vars are defined on entry
	 and used on exit *)
    let fd v = try VH.find defs v with Not_found -> (entryv,0) in 
    let cfg_to_ddg_vertex ddg cfg_vertex : SSA.G.V.t = 
      let lbl = SSA.G.V.label cfg_vertex in 
	SSA.find_vertex ddg lbl
    in
      VH.fold
	(fun var uselst ddg -> 
	   let (cfg_dv,_) = fd var in 
	   let ddg_dv = cfg_to_ddg_vertex ddg cfg_dv in
	     List.fold_left 
	       (fun ddg (cfg_uv,_) -> 
		  SSA.add_edge ddg 
		    (cfg_to_ddg_vertex ddg cfg_uv) ddg_dv) ddg uselst 
	) uses ddg 


  (** convert a cfg whose nodes contain any number of SSA stmts, to a
      cfg whose nodes contain a single SSA stmt in the node stmt
      list. This will make subsequent graphs more precise as an edge
      corresponds to a def/use statement, instead of a def/use block.
  *)
  let stmtlist_to_single_stmt cfg = 
    let translate_block old_v g : SSA.G.t= 
      let v = (SSA.find_vertex g (SSA.G.V.label old_v)) in 
      match (SSA.get_stmts g v)  with
	  [] -> g
	| s::[] -> g (* the first element in the list gets the original
			block id. Thus, we need to do nothing. *)
	| s::ss -> 
	    let origsucclst = SSA.G.succ g v in 
	    let g' = List.fold_left 
	      (fun g' dst -> SSA.remove_edge g' v dst) g origsucclst 
	    in 
	    let v',g' = 
	      List.fold_left 
		(fun (pred,g') s ->
		   let g',v' = SSA.create_vertex g' [s] in 
		   let () = dprintf "Created vertex %s for %s"
		     (bbid_to_string (SSA.G.V.label v'))
		     (Pp.ssa_stmt_to_string s) in 
		   let g' = SSA.add_edge g' pred v' in 
		     (v',g')
		) (v,g') ss 
	    in
	    let g' = SSA.set_stmts g' v [s] in 
	      List.fold_left
		(fun g' dst -> SSA.add_edge g' v' dst) g'  origsucclst
    in
    let g = SSA.copy  cfg in 
      SSA.G.fold_vertex translate_block cfg g
end

(* module PDG =  *)
(* struct *)

(*   type dependence = [`True | `Control] *)
 
(*   let compute_pdg cfg =  *)
(*     let vars,defs,uses = SSA_DDG.compute_dd cfg in  *)
(*     let cd  = SSA_CDG.compute_cd cfg in  *)
(*     let fd v = try VH.find defs v with Not_found -> (entryv,0) in  *)
(*     let pdg = SSA.copy cfg in  *)
(*     let pdg = SSA.G.fold_edges  *)
(* 	(fun src dst cfg' ->  *)
(* 	    let s = SSA.find_vertex cfg' (SSA.G.V.label src) in  *)
(* 	    let d = SSA.find_vertex cfg' (SSA.G.V.label dst) in  *)
(* 	      SSA.remove_edge cfg' s d *)
(* 	) cfg pdg *)
(*     in *)
(*     let add_dependencies v g =  *)
(*       let g =  *)
(* 	if C.G.in_degree cfg v > 0 then *)
(* 	  List.fold_left (fun g v' ->  *)
(* 			    let edge = SSA.G.E.create v' `Control v)  g (df v) *)
(* 	else g  *)
(*       in  *)
(* 	g *)
(*     in *)
(*       SSA.fold_vertex add_dependencies cfg pdg *)

(* end *)

(* module SSA_DDG = *)
(* struct *)
(*   module S = Var.VarSet *)
(*   module Lang = *)
(*   struct *)
(*     open Ssa *)
(*     type t = Ssa.stmt list *)


(*     let get_defs acc = function *)
(* 	Move(l,_,_) -> S.add l acc *)
(*       | _ -> acc *)
(*     let get_defs stmts = *)
(*       S.elements (List.fold_left get_defs S.empty stmts) *)

(*     let rec get_uses acc = *)
(*       let add_val_uses us = function *)
(* 	  Var l -> S.add l us *)
(* 	| _ -> us *)
(*       in *)
(*       let add_vals_uses us vals =  List.fold_left add_val_uses us vals in *)
(*       let add_exp_uses acc  = function *)
(* 	| UnOp(_,v) *)
(* 	| Val v *)
(* 	| Cast(_,_,v) *)
(* 	  -> add_val_uses acc  v *)
(* 	| BinOp(_,v1,v2) -> *)
(* 	    add_vals_uses acc [v1;v2] *)
(* 	| Load(v1,v2,v3,_) -> *)
(* 	    add_vals_uses acc [v1;v2;v3] *)
(* 	| Store(v1,v2,v3,v4,_) -> *)
(* 	    add_vals_uses acc [v1;v2;v3;v4] *)
(* 	| Phi vs  -> List.fold_left (fun acc v -> S.add v acc) acc vs *)
(* 	| Unknown _ -> acc *)
(*       in *)
(* 	function *)
(* 	  | Jmp(v,_) *)
(* 	  | Halt(v,_) *)
(* 	  | Assert(v,_) *)
(* 	    -> add_val_uses acc v *)
(* 	  | CJmp(v1,v2,v3,_) -> add_vals_uses acc [v1;v2;v3] *)
(* 	  | Move(_,e,_) -> add_exp_uses acc e *)
(* 	  | Label _ *)
(* 	  | Comment _ *)
(* 	    -> acc *)
(*     let get_uses stmts = *)
(*       S.elements (List.fold_left get_uses S.empty stmts) *)
	
(*   end *)

(*   module VH = Var.VarHash *)
(*   module G = C.G *)

(*   (\* the graph we will return *\) *)
(*   module DDG = Graph.Persistent.Digraph.Concrete(G.V) *)
(*   include DDG *)
  
  
(*   let compute_true_dependence cfg = *)
(*     let defs = VH.create 57 in *)
(*     let () = *)
(*       G.iter_vertex *)
(* 	(fun v -> let s = C.get_stmts cfg v in *)
(*           List.iter (fun var -> VH.add defs var v) (Lang.get_defs s)) *)
(* 	cfg *)
(*     in *)
(*     let vertices =  G.fold_vertex (fun v g -> add_vertex g v) cfg empty in *)
(*       G.fold_vertex *)
(* 	(fun v g -> *)
(*            List.fold_left *)
(*              (fun g var -> *)
(* 		try ( *)
(* 		  add_edge g (VH.find defs var) v *)
(* 		) with Not_found -> *)
(* 		  (wprintf "Can't find definition for %s" (Pp.var_to_string var); *)
(* 		   g) *)
(* 	     ) *)
(* 	     g *)
(* 	     (Lang.get_uses (C.get_stmts cfg v)) *)
(* 	) *)
(* 	cfg vertices *)

(* end *)

(* (\* type dependence = [`True | `Control] *\) *)

(* (\* module SSA_PDG  = *\) *)
(* (\* struct *\) *)
(* (\*   module Lang = struct type t = Ssa.stmt list end *\) *)
(* (\*   module G = C.G *\) *)

(* (\*   module Label = *\) *)
(* (\*   struct *\) *)
(* (\*     type t = dependence *\) *)
(* (\*     let compare = Pervasives.compare *\) *)
(* (\*     let default = `True *\) *)
(* (\*   end *\) *)
(* (\*   module PDG = Graph.Persistent.Digraph.ConcreteLabeled(G.V)(Label) *\) *)
(* (\*   include PDG *\) *)

(* (\*   module CDG = CDG(Lang) *\) *)
(* (\*   module DDG = SSA_DDG *\) *)

(* (\*   let compute_pdg cfg = *\) *)
(* (\*     let cdg = CDG.compute_cdg cfg in *\) *)
(* (\*     let ddg = DDG.compute_true_dependence cfg in *\) *)
(* (\*     let vertices =  G.fold_vertex (fun v g -> add_vertex g v) cfg empty in *\) *)
(* (\*     let withddg = *\) *)
(* (\*       DDG.fold_edges *\) *)
(* (\* 	(fun v v' g -> add_edge_e g (v, `True, v')) *\) *)
(* (\* 	ddg *\) *)
(* (\* 	vertices *\) *)
(* (\*     in *\) *)
(* (\*     let pdg = *\) *)
(* (\*       CDG.fold_edges *\) *)
(* (\* 	(fun v v' g -> *\) *)
(* (\* 	   if not(mem_edge g v v') *\) *)
(* (\* 	   then add_edge_e g (v, `Control, v') *\) *)
(* (\* 	   else g *\) *)
(* (\* 	) *\) *)
(* (\* 	cdg *\) *)
(* (\* 	withddg *\) *)
(* (\*     in *\) *)
(* (\*       (\\* FIXME: Remove other unneeded control dependences from the graph, as *\) *)
(* (\* 	 is traditional, according to the Muchnick book. *\\) *\) *)
(* (\*       pdg *\) *)

(* (\*   module REVG = *\) *)
(* (\*   struct *\) *)
(* (\*     type t = PDG.t *\) *)
(* (\*     module V = PDG.V *\) *)
(* (\*     module G = PDG *\) *)
(* (\*     type vertex = PDG.V.t *\) *)
      
(* (\*     let pred = PDG.succ *\) *)
(* (\*     let succ  = PDG.pred *\) *)
(* (\*     let fold_vertex = PDG.fold_vertex *\) *)
(* (\*     let iter_vertex = PDG.iter_vertex *\) *)
(* (\*     let iter_succ = PDG.iter_pred *\) *)
(* (\*     let remove_vertex = PDG.remove_vertex  *\) *)
(* (\*   end *\) *)

(* (\*   module REVNP = Prune_unreachable.Make(REVG);; *\) *)

(* (\*   let slice g v  = *\) *)
(* (\*     REVNP.reachable g v *\) *)
      
(* (\* end *\) *)


(* (\* module PdgSsaStmtsPrinter = *\) *)
(* (\* struct *\) *)
(* (\*   module Helper = *\) *)
(* (\*     Vine_graphviz.MakeOtherCfgPrinter(SSA_PDG)(Ssa.G)(Vine_graphviz.PrintSsaStmts) *\) *)
(* (\*   include Helper *\) *)

(* (\*   let edge_attributes (((_,t,_),_): E.t) =  *\) *)
(* (\*     match t with *\) *)
(* (\* 	`Control -> [`Style `Dashed] *\) *)
(* (\*       | `True -> [] *\) *)
(* (\* end *\) *)

(* (\* module PdgSsaStmtDot = Graph.Graphviz.Dot(PdgSsaStmtsPrinter) *\) *)

