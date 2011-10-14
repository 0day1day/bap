(** Hacks *)

open Type
open Ast
open Util

module C = Cfg.AST
module D = Debug.Make(struct let name = "Hacks" and default=`Debug end)
open D

let ra_final = Var.newvar "ra_final" reg_32
and ra0 = Var.newvar "ra0" Ast.reg_32
and (mem,sp,r_of) =
  let d = Asmir.decls_for_arch Asmir.arch_i386 in
  (List.hd d,
   List.find (fun v -> Var.name v = "R_ESP") d,
   List.find (fun v -> Var.name v = "R_OF") d
  )

let function_end = "function_end"
and attrs = [StrAttr "ret hack"]
let save_ra0 = Move(ra0, Load(Var mem, Var sp, exp_false, reg_32), attrs)

let ret_to_jmp ?(ra=ra_final) p =
  let a = Array.of_list p in
  Array.iteri
    (fun i s -> match s with
      (* Old lifting of ret *)
     | Special("ret", _) ->
	 a.(i) <- Jmp(Lab function_end, attrs);
	 (match a.(i-1) with 
	  | Jmp(t,at) -> a.(i-1) <- Move(ra, t, attrs@at)
	  | _ -> failwith "expected Jmp before ret special"
	 )
     (* disasm_i386 lifting of ret *)
     | Jmp(t, attrs) when attrs = [StrAttr "ret"] ->
       a.(i) <- Jmp(Lab function_end, attrs)
     | _ -> ()
    ) a;
  let l = Array.to_list a in
  save_ra0::l@[Label(Name function_end, attrs)]


let attrs = [StrAttr "noof hack"]
let assert_noof p =
  let il = List.map
    (function
       | Move(v, e, a) as s when v == r_of ->
	   [s; Assert(exp_not (Var v), attrs)]
       | s -> [s]
    ) p
  in
  List.flatten il


let remove_cycles cfg =
  let module C = Cfg.AST in
  let a = StrAttr "added by remove_cycles" in
  (* let assert_false = Assert(exp_false, a) in *)
  let assert_false = Jmp(Lab("Bb_error"), a::[]) in
  let cfg, error = Cfg_ast.find_error cfg in
  let handle_backedge cfg e =
    let s = C.G.E.src e in
    let l = C.G.E.label e in
    let revstmts = List.rev (C.get_stmts cfg s) in
    let revstmts = match revstmts with
      | Jmp(t,_)::rest ->
	  assert_false::rest
      | CJmp(c,t1,t2,attrs)::rest ->
	(* e is the label we are REMOVING *)
	(match l with
	| Some true -> CJmp(c, Lab("BB_Error"), t2, a::attrs)
	| Some false -> CJmp(c, t1, Lab("BB_Error"), a::attrs)
	| None -> failwith "missing edge label from cjmp")
          ::rest
      | rest -> assert_false::rest
    in
    let cfg = C.set_stmts cfg s (List.rev revstmts) in
    let cfg = C.remove_edge_e cfg e in
    let newedge = C.G.E.create s l (C.G.V.create Cfg.BB_Error) in
    let cfg = C.add_edge_e cfg newedge in
    cfg
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
      edges
    in
    walk entry []
  in
  dprintf "Finding edges";
  let backedges = find_backedges cfg in
  (* I don't think we need this anymore... *)
  (* let backedges = Util.list_unique backedges in *)
  List.fold_left handle_backedge cfg backedges

(** Fix outgoing edges of [n] in [g] *)
let repair_node g n =
  let g, error = Cfg_ast.find_error g in
  let g, exit = Cfg_ast.find_exit g in
  let g, indirect = Cfg_ast.find_indirect g in
  (* Some of this is copied from Cfg_ast.of_prog *)
  let make_edge g v ?lab t =
    let dst = lab_of_exp t in
    let tgt = match dst with
      | None -> indirect
      | Some l ->
	  try (C.find_label g l)
	  with Not_found ->
	    wprintf "Jumping to unknown label: %s" (Pp.label_to_string l);
	    error
      (* FIXME: should jumping to an unknown address be an error or indirect? *)
    in
    C.add_edge_e g (C.G.E.create v lab tgt)
  in
  let edges = C.G.succ_e g n in
  let g = List.fold_left C.remove_edge_e g edges in
  let stmts = C.get_stmts g n in
  match List.rev stmts with
  | Jmp(t, _)::_ -> make_edge g n t
  | CJmp(_,t,f,_)::_ -> let g = make_edge g n ~lab:true t in
                     make_edge g n ~lab:false f
  | Special _::_ -> C.add_edge g n error
  | Halt _::_ -> C.add_edge g n exit
  | _ -> (* It's probably fine.  That's why this is in hacks.ml. *) g

(** Repair cfg whose graph is inconsistent with its statements *)
let repair_cfg g =
  (* XXX: Better implementation *)
  let p = Cfg_ast.to_prog g in
  let oc = open_out "p.out" in
  let pp = new Pp.pp_oc oc in
  pp#ast_program p;
  pp#close;
  let cfg = Cfg_ast.of_prog p in
  let cfg, entry = Cfg_ast.find_entry cfg in
  Reachable.AST.remove_unreachable cfg entry
