open Type
open Ast
open Util

let ret_to_jmp p =
  let function_end = "function_end"
  and attrs = [StrAttr "ret hack"] in
  let a = Array.of_list (List.map (fun x -> Some x) p) in
  Array.iteri
    (fun i s -> match s with
     | Some(Special("ret", _)) ->
	 a.(i-1) <- Some(Jmp(Lab function_end, attrs));
	 a.(i) <- None
     | _ -> ()
    ) a;
  let l = list_filter_some id (Array.to_list a) in
  l@[Label(Name function_end, attrs)]



let remove_backedges cfg =
  let module C = Cfg.AST in
  let a = [StrAttr "added by remove_backedeges"] in
  let assert_false = Assert(exp_false, a) in
  let handle_backedge cfg e =
    let s = C.G.E.src e in
    let revstmts = List.rev (C.get_stmts cfg s) in
    let revstmts = match revstmts with
      | Jmp(t,_)::rest ->
	  assert_false::rest
      | CJmp(c,t1,t2,attrs)::rest ->
	  let (t,c) = match C.G.E.label e with
	    | Some true -> (t1, c)
	    | Some false -> (t2, exp_not c)
	    | None -> failwith "missing edge label from cjmp"
	  in
	Jmp(t, attrs)::Assert(c,a)::rest
      | rest -> assert_false::rest
    in
    let cfg = C.set_stmts cfg s (List.rev revstmts) in
    C.remove_edge_e cfg e
  in
  let find_backedges cfg =
    let module H = Hashtbl.Make(Cfg.BBid) in
    let h = H.create (C.G.nb_vertex cfg)
    and entry = C.find_vertex cfg Cfg.BB_Entry in
    let color v = try H.find h (C.G.V.label v) with Not_found -> false
    and setcolor v c = H.replace h (C.G.V.label v) c in
    let rec walk v edges=
      let walk_edge e edges =
	let d = C.G.E.dst e in
	if color d then e::edges
	else walk d edges
      in
      setcolor v true;
      let edges = C.G.fold_succ_e walk_edge cfg v edges in
      setcolor v false;
      edges
    in
    walk entry []
  in
  let backedges = find_backedges cfg in
  List.fold_left handle_backedge cfg backedges
