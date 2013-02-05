(** Loop unrolling *)
open Type
open Ast
open BatPervasives
module C = Cfg.AST
module D = Debug.Make(struct let name = "Unroll" and default=`NoDebug end)
open D

(* Type of unroll function *)
type unrollf = ?count:int -> Cfg.AST.G.t -> Cfg.AST.G.t

(* Generic nested loop information *)
type loopinfo =
  | BB of C.G.V.t
  | Other of loopinfo list
  | Loop of C.G.V.t * loopinfo list (* head, body of loop including head *)

(* XXX: inefficient *)
let rec bbs_from_loopinfo = function
  | BB v -> [v]
  | Other l | Loop (_, l) -> BatList.concat (List.map bbs_from_loopinfo l)

(* Get loop information from structural analysis *)
let loopinfo_from_sa cfg =
  let module SA = Structural_analysis in
  let module Dom = Dominator.Make(C.G) in
  let bbs_of_node =
    let rec get_nodes acc = function
      | SA.BBlock b -> (C.G.V.create b)::acc
      | SA.Region(_, ns) -> List.fold_left get_nodes acc ns
    in
    get_nodes []
  in
  let rec conv : SA.node -> loopinfo = function
    | SA.BBlock b -> BB (C.G.V.create b)
    | SA.Region((SA.SelfLoop | SA.WhileLoop | SA.NaturalLoop), ns) as r ->
      let nodes = bbs_of_node r in
      let h = Hashtbl.create (List.length nodes) in
      List.iter (fun n -> Hashtbl.add h n ()) nodes;
      let find_pred r =
        (* Find a bb that has a predecessor not in h *)
        try Some (List.find (fun bb ->
          List.exists (fun bb' ->
            Hashtbl.mem h bb' = false) (C.G.pred cfg bb)
        ) (bbs_of_node r))
        with Not_found -> None
      in
      let tl = List.map find_pred ns in
      let tl = BatList.filter_map Util.id tl in
      let head = match tl with
        | [hd] -> hd
        | _ -> failwith "loopinfo_from_sa: Failed to unroll irreducible loop"
      in
      Loop(head, List.map conv ns)
    | SA.Region(_, ns) -> Other(List.map conv ns)
  in
  conv (SA.structural_analysis cfg)

let unroll_loop ?(count=8) ?(id=0) cfg head body =
  dprintf "Unrolling loop for %s with %d nodes" (Cfg.bbid_to_string (C.G.V.label head)) (List.length body);
  let nodes = head::body in
  let nnodes = List.length nodes in
  let ohead = head in
  let edges = List.fold_left
    (fun acc node ->
      List.fold_left
        (fun acc outedge ->
          let dst = C.G.E.dst outedge in
            (*if List.mem dst nodes then*)
            let lab = C.G.E.label outedge in
            (node,lab,dst)::acc 
            (*else acc*)
         ) acc (C.G.succ_e cfg node)
    ) [] nodes
  in
  let unrollednodes = Hashtbl.create (nnodes * count) in
  let backedges = Hashtbl.create (nnodes * count) in
  let renewlabel l i = l ^ "_unroll_" ^ string_of_int i ^ "_" ^ string_of_int id in
  let duplicate cfg i =
    List.fold_left 
      (fun cfg node ->
         let stmts = C.get_stmts cfg node in
         let stmts =
           List.map (function
		       | Label(Name l, a) -> Label(Name(renewlabel l i), a)
		       | Label(Addr addr, a) -> Label(Name(Printf.sprintf "addr_%Lx_unroll_%d_%d" addr i id), a)
		       | s -> s
	            ) stmts
         in
         let (cfg, node') = C.create_vertex cfg stmts in
         Hashtbl.add unrollednodes (C.G.V.label node, i) node';
         Hashtbl.add backedges node' node;
         cfg
      ) cfg nodes
  in
  let ith_copy i vertex =
    try Hashtbl.find unrollednodes (C.G.V.label vertex, i)
    with Not_found -> 
     (*failwith ("No copy found for " 
                ^ (string_of_int i) 
                ^ "-th copy of vertex " 
                ^ (Cfg.bbid_to_string (C.G.V.label vertex)));*)
      dprintf "No copy found for %s-th copy" (string_of_int i);
      dprintf "of vertex %s " (Cfg.bbid_to_string (C.G.V.label vertex));
      vertex
  in
  let fix_backedge ?(final=false) cfg i =
    dprintf "Fixing backedge %d" i;
    let head, nodes =
      if i = 1 then head, nodes (* fix backedge for original head *)
      else 
        let previous = i - 1 in
        let previous_nodes = List.map (ith_copy previous) nodes in
        ith_copy previous head, previous_nodes
    in
    let incomingedges = C.G.pred_e cfg head in
    List.fold_left 
      (fun cfg edge ->
         let src = C.G.E.src edge
         and dst = C.G.E.dst edge in
         if List.mem src nodes then
           (
             dprintf "Removing edge";
             let cfg = C.remove_edge_e cfg edge in
             let newdst =
               if not final then
                 let origdst = try Hashtbl.find backedges dst with Not_found -> dst in
                 ith_copy i origdst
               else
		 ohead (* C.find_vertex cfg (Cfg.BB_Exit) *)
             in
             let edgelabel = C.G.E.label edge in
             let newedge = C.G.E.create src edgelabel newdst in
             C.add_edge_e cfg newedge
           )
         else
           cfg
      ) cfg incomingedges
  in
  let dup_edges cfg i =
    List.fold_left 
      (fun cfg (src, lab, dst) ->
         let src = ith_copy i src in
              
         let dst = ith_copy i dst in
         let newedge = C.G.E.create src lab dst in
         C.add_edge_e cfg newedge
      ) cfg edges
  in
  let unroll_once cfg i =
    dprintf "Unrolling %d" i;
    let cfg = duplicate cfg i in
    let cfg = fix_backedge cfg i in
    let cfg = dup_edges cfg i in
    cfg
  in
  let cfg = fold unroll_once cfg (1--count) in
  let cfg = fix_backedge ~final:true cfg (count+1) in

  let rename_targets vertex cfg =
    let getlabel origlabel node =
      let label = match lab_of_exp origlabel with Some x -> x | _ -> failwith "indirect" in
      if C.find_label cfg label == node then origlabel
      else
	match C.get_stmts cfg node with
	  | Label(label,_)::_ -> exp_of_lab label
	  | stmt::_ -> failwith ("missing replacement label FIXME " ^ (Pp.ast_stmt_to_string stmt)) (* This could happen if l was an Addr *)
          | [] -> failwith "Empty node"
    in
    let revstmts = List.rev (C.get_stmts cfg vertex) in
    let revstmts' = match revstmts with
      | (CJmp(c,t1,t2,attrs) as stmt)::rest ->
	  let e1,e2 = match C.G.succ_e cfg vertex with
	    | [e1;e2] when C.G.E.label e1 = Some true && C.G.E.label e2 = Some false ->
		(e1,e2)
	    | [e1;e2] when C.G.E.label e2 = Some true && C.G.E.label e1 = Some false ->
		(e2,e1)
	    | _ ->
                failwith ("Something is wrong with the edges or edge labels:"^(Pp.ast_stmt_to_string stmt))
	  in
	  let s1 = C.G.E.dst e1 and s2 = C.G.E.dst e2 in
          (*dprintf "Generating e1 and e2";*)
	  let t1' = getlabel t1 s1 and t2' = getlabel t2 s2 in
          (*dprintf "Found: %s =?= %s" (Pp.ast_exp_to_string t1') (Pp.ast_exp_to_string t2');*)
          (*if (t1' = t2') then
            (let ss = C.get_stmts cfg s1 in dprintf "stmt: %s" (Pp.ast_stmt_to_string (List.hd ss));
             let ss = C.get_stmts cfg s2 in dprintf "stmt: %s" (Pp.ast_stmt_to_string (List.hd ss)));*)
	  if t1' === t1 && t2' === t2 then revstmts
	  else CJmp(c,t1',t2',attrs)::rest
      | Jmp _::rest
      | rest -> rest
    in
    if revstmts == revstmts' then cfg
    else C.set_stmts cfg vertex (List.rev revstmts')
  in

  let cfg = C.G.fold_vertex rename_targets cfg cfg in
  let nodelist = Hashtbl.fold (fun k v l ->
    v::l
  ) unrollednodes nodes in

  (* let oc = Pervasives.open_out "unroll.dot" in *)
  (* (\*let ssa_func_cfg = Cfg_ssa.of_astcfg cfg in *)
  (* Cfg_pp.SsaStmtsDot.output_graph oc ssa_func_cfg;*\) *)
  (* Cfg_pp.AstBBidDot.output_graph oc cfg; *)
  (* Pervasives.close_out oc; *)
  cfg, nodelist
(*
  let rename_targets cfg v =
    let getlabel le n =
      let l = match lab_of_exp le with Some x -> x | _ -> failwith "indirect" in
      if C.find_label cfg l == n then le
      else
	let rec find_label = function
	  | Label(l,_)::_ -> exp_of_lab l
	  | Comment _ :: xs -> find_label xs
	  | _ -> failwith "missing replacement label FIXME" (* This could happen if l was an Addr *)
	in
	find_label (C.get_stmts cfg v)
    in
    let revstmts = List.rev (C.get_stmts cfg v) in
    let revstmts' = match revstmts with
      | (CJmp(c,t1,t2,attrs) as stmt)::rest ->
	  let e1,e2 = match C.G.succ_e cfg v with
	    | [e1;e2] when C.G.E.label e1 = Some true && C.G.E.label e2 = Some false ->
		(e1,e2)
	    | [e1;e2] when C.G.E.label e2 = Some true && C.G.E.label e1 = Some false ->
		(e2,e1)
	    | _ ->
                let oc = open_out "test1" in
                let ssa_func_cfg = Cfg_ssa.of_astcfg cfg in
                Cfg_pp.SsaStmtsDot.output_graph oc ssa_func_cfg;
                (*Cfg_pp.AstBBidDot.output_graph oc cfg;*)
                close_out oc;
		failwith ("Something is wrong with the edges or edge labels:"^(Pp.ast_stmt_to_string stmt))
	  in
	  let s1 = C.G.E.dst e1 and s2 = C.G.E.dst e2 in
	  let t1' = getlabel t1 s1 and t2' = getlabel t2 s2 in
	  if t1' = t1 && t2' = t2 then revstmts
	  else CJmp(c,t1',t2',attrs)::rest
      | Jmp _::rest
      | rest ->
	  rest
    in
    if revstmts == revstmts' then cfg
    else C.set_stmts cfg v (List.rev revstmts')
  in

  cfg

*)
let unroll_bbs ?count ?id cfg head bbs =
  dprintf "unroll_bbs invoked";
  let body = List.filter ((<>)head) bbs in
  unroll_loop ?count ?id cfg head body


let unroll_loops_internal ?count cfg loopinfo =
  let () = Checks.connected_astcfg cfg "unroll_loops_internal" in
  let nunrolled = ref 0 in

  (* unroll_in cfg needs to return a CFG and a modified structure,
     since unrolling changes the structure of the CFG. *)
  let rec unroll_in cfg : loopinfo -> C.G.t * loopinfo =
    let f (cfg,rl) n =
      let cfg,r = unroll_in cfg n in
      cfg, r::rl
    in
    function
    | BB _ as r -> cfg, r
    | Other ns ->
      (* Recurse *)
      let cfg, rl = List.fold_left f (cfg,[]) ns in
      cfg, Other(List.rev rl)
    | Loop(head, ns) ->
      (* First unroll any nested loops *)
      let cfg, r =
        let cfg, rl = List.fold_left f (cfg,[]) ns in
        cfg, Other(List.rev rl)
      in

      (* And then unroll the loop at the current level *)
      let bbs = bbs_from_loopinfo r in
      dprintf "Found a loop with %d nodes" (List.length bbs);
      (* We need to return an updated region for the unrolled loop. *)
      let cfg, nl = unroll_bbs ?count ~id:!nunrolled cfg head bbs in
      incr nunrolled;
      let make_region bb = BB bb in
      cfg, Other (List.map make_region nl)
  in
  let cfg, _ = unroll_in cfg loopinfo in
  cfg

let unroll_loops_sa ?count cfg =
  unroll_loops_internal ?count cfg (loopinfo_from_sa cfg)

let unroll_loops = unroll_loops_sa
