(** Loop unrolling *)
open Type
open Ast
module C = Cfg.AST
module D = Debug.Make(struct let name = "Unroll" and default=`Debug end)
open D


let unroll_loop ?(count=8) cfg head body =
  dprintf "Unrolling loop for %s with %d nodes" (Cfg.bbid_to_string (C.G.V.label head)) (List.length body);
  let nodes = head::body in
  let nnodes = List.length nodes in
  let copies = Hashtbl.create (nnodes * count)
  and orig = Hashtbl.create nnodes in
  List.iter (fun n -> Hashtbl.add orig n ()) nodes;
  let renewlabel l i =
    (* HACK for now *)
    l^"_v"^string_of_int i
  in
  let dup i cfg n =
    let stmts = C.get_stmts cfg n in
    let stmts =
      List.map (function
		  | Label(Name l, a) -> Label(Name(renewlabel l i), a)
		  | Label(Addr i, a) -> Comment(Printf.sprintf "Was addr %Lx" i, a)
		  | s -> s
	       ) stmts
    in
    let (cfg, n') = C.create_vertex cfg stmts in
    Hashtbl.add copies (C.G.V.label n, i) n';
    cfg
  in
  let ni2n' src n i =
    if n = head && not src then
      if i = count then
	n
      else Hashtbl.find copies (C.G.V.label n, i+1)
    else
      try Hashtbl.find copies (C.G.V.label n, i)
      with Not_found -> n
  in
  let fix_backedge cfg backedge =
    let be_src = C.G.E.src backedge and be_l = C.G.E.label backedge in
    if Hashtbl.mem orig be_src then
      let cfg = C.remove_edge_e cfg backedge in
      C.add_edge_e cfg (C.G.E.create be_src be_l (ni2n' false head 0))
    else cfg
  in
  let add_edges cfg n =
    let add_edge cfg e =
      let l = C.G.E.label e
      and d = C.G.E.dst e in
      assert(C.G.E.src e = n);
      Util.foldn ~t:1
	(fun cfg i ->
	   let e' = C.G.E.create (ni2n' true n i) l (ni2n' false d i) in
	   C.add_edge_e cfg e'
	)
	cfg (count-1)
    in
    List.fold_left add_edge cfg (C.G.succ_e cfg n)
  in
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
      | CJmp(c,t1,t2,attrs)::rest ->
	  let e1,e2 = match C.G.succ_e cfg v with
	    | [e1;e2] when C.G.E.label e1 = Some true && C.G.E.label e2 = Some false ->
		(e1,e2)
	    | [e1;e2] when C.G.E.label e2 = Some true && C.G.E.label e1 = Some false ->
		(e2,e1)
	    | _ ->
		failwith "Something is wrong with the edges or edge labels"
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

  let cfg = ref cfg in
  for i = 1 to count-1 do
    cfg := List.fold_left (dup i) !cfg nodes
  done;
  let cfg = List.fold_left fix_backedge !cfg (C.G.pred_e !cfg head) in
  let cfg = List.fold_left add_edges cfg nodes in
  List.iter (fun n -> Hashtbl.add copies (C.G.V.label n, 0) n) nodes;
  let cfg = Hashtbl.fold (fun k v c -> rename_targets c v) copies cfg in
  cfg


let unroll_bbs ?count idom cfg bbs =
  let nodes = List.map (C.find_vertex cfg) bbs in
  let h = Hashtbl.create (List.length nodes) in
  List.iter (fun n -> Hashtbl.add h n ()) nodes;
  let rec findhead n =
    let n' = idom n in
    if Hashtbl.mem h n' then findhead n'
    else n
  in
  let head = findhead (List.hd nodes) in
  let body = List.filter ((<>)head) nodes in
  unroll_loop ?count cfg head body


let unroll_loops ?count cfg =
  let module SA = Structural_analysis in
  let module Dom = Dominator.Make(C.G) in
  let idom = Dom.compute_idom cfg (C.find_vertex cfg Cfg.BB_Entry) in
  let bbs_of_node =
    let rec f l = function
      | SA.B b -> b::l
      | SA.R(_, ns) -> List.fold_left f l ns
    in
    f []
  in
  let rec unroll_in cfg = function
    | SA.B _ -> cfg
    | SA.R(rt, ns) as n ->
	(* Don't worry about nested loops for now... *)
	let cfg = match rt with
	  | SA.SelfLoop | SA.WhileLoop | SA.NaturalLoop ->
	      let bbs = bbs_of_node n in
	      dprintf "Found a loop with %d nodes" (List.length bbs);
	      unroll_bbs ?count idom cfg bbs
	  | _ -> cfg
	in
	List.fold_left unroll_in cfg ns
  in
  unroll_in cfg (SA.structural_analysis cfg)
