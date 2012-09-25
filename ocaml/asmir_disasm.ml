open Ast
open Ast_convenience
open Big_int_convenience
module CA = Cfg.AST
module D = Debug.Make(struct let name = "Asmir_disasm" and default=`NoDebug end)
open D
open Type

(* XXX: Handle conditional function calls *)
(* VSA dataflow reuse *)
(* Call/ret behavior *)

type succs = | Addrs of label list
             | Error
             | Exit
             | Indirect

module type STATE = sig
  type t
  val init : t
end

module NOOPSTATE = struct
  type t = unit
  let init = ()
end

module type FUNCID = sig
  module State : STATE
  val find_calls : Cfg.AST.G.t -> Cfg.AST.G.V.t list -> Cfg_ast.unresolved_edge list -> State.t -> Cfg_ast.unresolved_edge list * State.t
  val find_rets : Cfg.AST.G.t -> Cfg.AST.G.V.t list -> Cfg_ast.unresolved_edge list -> State.t -> Cfg_ast.unresolved_edge list * State.t
end

module FUNCFINDER_DUMB = struct
  module State = NOOPSTATE
  let is_call_stmt = function
    | Jmp(_, [StrAttr "call"]) -> true
    | _ -> false
  let is_ret_stmt = function
    | Jmp(_, [StrAttr "ret"]) -> true
    | _ -> false
  let check_last f c nodes unresolved_edges () =
    List.filter (fun (v,_,_) -> let stmts = CA.get_stmts c v in
                          match List.rev stmts with
                          | last::_ -> f last
                          | [] -> false) unresolved_edges, ()
  let find_calls = check_last is_call_stmt
  let find_rets = check_last is_ret_stmt
end

module type DISASM = sig
  module State : STATE
  (* val visit_new_address : addr * string * Cfg.AST.G.V.t list * Cfg.AST.G.E.t list * Cfg.AST.G.V.t option -> State.t -> State.t *)
  (* visit_new_edge? *)
  val get_succs : Asmir.asmprogram -> Cfg.AST.G.t -> Cfg.AST.G.V.t * Cfg.AST.G.E.label * Ast.exp -> State.t -> succs * State.t (** Function that returns the successors of a node *)
end

module RECURSIVE_DESCENT_SPEC = struct
  module State = struct
    type t = unit
    let init = ()
  end
  let get_succs _asmp g (v,l,e) () =
    let o = match List.rev (CA.get_stmts g v), l with
      | last::_, None when FUNCFINDER_DUMB.is_ret_stmt last ->
        Exit
      | CJmp _::_, Some _ ->
        (match lab_of_exp e with
        | Some l -> Addrs [l]
        | None -> Indirect)
      | CJmp _::_, _ -> failwith "error"
      (* Fallthrough/Jmp *)
      | _::_, None ->
        (match lab_of_exp e with
        | Some l -> Addrs [l]
        | None -> Indirect)
      | _::_, Some _ -> failwith "error"
      | [], _ -> Addrs []
    in
    o, ()
end

module VSA_SPEC = struct
  module State = struct
    type t = unit
    let init = ()
  end
  let get_succs asmp g (v,l,e) () =
    match RECURSIVE_DESCENT_SPEC.get_succs asmp g (v,l,e) () with
    | Indirect, () ->
      (* Do VSA stuff *)
      if l <> None then failwith "VSA-enabled lifting currently assumes that conditional jumps are not indirect";
      let cfg = Hacks.ast_exit_indirect (CA.copy g) in
      let cfg = Ast_cond_simplify.simplifycond_cfg cfg in
      (* Cfg_pp.AstStmtsDot.output_graph (open_out "vsa.dot") cfg; *)
      let _df_in, df_out = Vsa.AlmostVSA.DF.worklist_iterate_widen ~nmeets:50 ~opts:{Vsa.AlmostVSA.DFP.O.initial_mem=Asmir.get_readable_mem_contents_list asmp} cfg in
      let vs = Vsa.AlmostVSA.DFP.exp2vs (df_out v) e in
      dprintf "VSA resolved %s to %s" (Pp.ast_exp_to_string e) (Vsa.VS.to_string vs);
      (match Vsa.VS.concrete ~max:50 vs with
      | Some x -> Addrs (List.map (fun a -> Addr a) x), ()
      | None -> wprintf "VSA disassembly failed to resolve %s to a specific concrete set" (Pp.ast_exp_to_string e); Indirect, ())
      (* Rely on recursive descent for easy stuff *)
    | o, () -> o, ()

end

module Make(D:DISASM)(F:FUNCID) = struct
  let disasm_at p addr =
    let (tmp, entry) = Cfg_ast.create_entry (CA.empty()) in
    let (tmp, exit) = Cfg_ast.find_exit tmp in
    let (tmp, error) = Cfg_ast.find_error tmp in
    let (c, indirect) = Cfg_ast.find_indirect tmp in
    let succh = Hashtbl.create 1000 in
    let c = CA.add_edge c indirect error in (* indirect jumps could fail *)
    let c = ref c in
    let state = ref D.State.init in
    let q = Queue.create () in
    (* bbs of lifted insruction, unresolved outgoing edges from lifted instruction, successor address *)
    let () = Queue.add (entry::[], (entry,None,exp_of_lab (Addr addr))::[], addr) q in
    let raise_address c a =
      dprintf "raise_address %#Lx" a;
      try c, CA.find_label c (Addr a)
      with Not_found ->
        let (prog, next) = Asmir.asm_addr_to_bap p a in
        Hashtbl.replace succh a next;

        let (c', edges, bbs, fallthrough) = Cfg_ast.add_prog c prog in

        (* Resolve edges internal to this instruction.  All internal
           edges will be to Name labels (instead of Addr labels).

           We need to add internal edges here so we can actually do
           analysis on the CFG (for example, concretely executing to
           see if there is a call).
        *)
        let c', edges = List.fold_left
          (fun (c,unresolved_edges) ((s,l,e) as edge) ->
            match lab_of_exp e with
            | Some (Name _ as t) -> CA.add_edge_e c (CA.G.E.create s l (CA.find_label c t)), unresolved_edges
            | _ -> c, edge::unresolved_edges
          ) (c',[]) edges in

        (* Queue edges *)
        let edges = match fallthrough with
          | Some v -> (v,None,exp_of_lab (Addr next))::edges
          | None -> edges
        in
        Queue.add (bbs, edges, next) q;
        c', CA.find_label c' (Addr a)
    in
    (* addcond: Add condition to edge?
       (s,l,e): original edge information
       c: graph
       lbl: resolved edge destination
    *)
    let add_resolved_edge addcond (s,l,e) c lbl  =
      try
        let c, dst = match lbl with
          | Addr a -> raise_address c a
          | Name s -> failwith "add_resolved_edge: Named labels should be resolved in raise_address"
        in
        let l' = match addcond, lbl with
          | true, Addr a ->
            if l <> None then failwith "add_resolved_edge: Indirect conditional jumps are unimplemented";
            Some(true (* XXX: This is meaningless! *), binop EQ e (Int(bi64 a, Typecheck.infer_ast ~check:false e)))
          | true, Name _ -> failwith "add_resolved_edge: It is not possible to resolve indirect jump to a named label"
          | false, _ -> l
        in

        CA.add_edge_e c (CA.G.E.create s l' dst)
      with Asmir.Memory_error ->
        (* Should this go to error or exit? *)
        wprintf "Raising address %s failed" (Pp.label_to_string lbl);
        let c,error = Cfg_ast.find_error c in
        CA.add_edge_e c (CA.G.E.create s l error)
    in
    (* Side effects: Adds to queue *)
    let process_edge (c,state) ((s,l,t) as e) =
      dprintf "Looking at edge from %s to %s" (Cfg_ast.v2s s) (Pp.ast_exp_to_string t);
      let resolved_addrs,state' = D.get_succs p c e state in
      match resolved_addrs with
      | Addrs addrs ->
        dprintf "%d Addrs" (List.length addrs);
        let addcond = List.length addrs > 1 in
        List.fold_left (add_resolved_edge addcond e) c addrs, state'
      | Error | Exit | Indirect ->
        dprintf "Special";
        let c', dest = match resolved_addrs with
          | Error -> Cfg_ast.find_error c
          | Exit -> Cfg_ast.find_exit c
          | Indirect -> Cfg_ast.find_indirect c
          | _ -> failwith "impossible"
        in
        CA.add_edge_e c' (CA.G.E.create s l dest), state'
    in
    (* Main loop *)
    while not (Queue.is_empty q) do
      let (bbs, unresolved_edges, succ) = Queue.pop q in
      let c', unresolved_edges = match F.find_calls !c bbs unresolved_edges F.State.init with
        | [], _ -> !c, unresolved_edges
        | call_edges, _ ->
          (* We found edges corresponding to calls *)
          let others = Util.list_difference unresolved_edges call_edges in
          let fallthrough = List.map (fun (s,l,e) -> (s,l, exp_of_lab (Addr succ))) call_edges in
          let dumb_translate_call cfg (s,l,e) =
            let revstmts = match List.rev (CA.get_stmts !c s) with
              | CJmp _::_ -> failwith "Conditional function calls are not implemented"
              | Jmp _ as s::tl -> Comment(Printf.sprintf "Function call removed: %s" (Pp.ast_stmt_to_string s), [])::tl
              | _ -> failwith "Unable to rewrite function call"
            in
            CA.set_stmts cfg s (List.rev revstmts)
          in
          let c' = List.fold_left dumb_translate_call !c call_edges in
          c', BatList.append others fallthrough
      in
      let c',state' = List.fold_left process_edge (c',!state) unresolved_edges in
      c := c';
      state := state'
    done;

    !c, !state

  let disasm p = disasm_at p (Asmir.get_start_addr p)
end

let recursive_descent =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC)(FUNCFINDER_DUMB) in
  RECURSIVE_DESCENT.disasm

let recursive_descent_at =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC)(FUNCFINDER_DUMB) in
  RECURSIVE_DESCENT.disasm_at

let vsa =
  let module VSA = Make(VSA_SPEC)(FUNCFINDER_DUMB) in
  VSA.disasm

let vsa_at =
  let module VSA = Make(VSA_SPEC)(FUNCFINDER_DUMB) in
  VSA.disasm_at
