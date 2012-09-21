open Ast
module CA = Cfg.AST
module D = Debug.Make(struct let name = "Asmir_disasm" and default=`NoDebug end)
open D
open Type

type succs = | Addrs of label list
             | Error
             | Exit
             | Indirect

module type DISASM = sig
  module State : sig
    type t (** Auxiliary data that is returned after disassembly *)
    val init : t
  end
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
      (match Vsa.VS.concrete ~max:50 vs with
      | Some x -> Addrs (List.map (fun a -> Addr a) x), ()
      | None -> wprintf "VSA disassembly failed"; Indirect, ())
      (* Rely on recursive descent for easy stuff *)
    | o, () -> o, ()

end

module Make(D:DISASM) = struct
  let disasm_at p addr =
    let (tmp, entry) = Cfg_ast.create_entry (CA.empty()) in
    let (tmp, exit) = Cfg_ast.find_exit tmp in
    let (tmp, error) = Cfg_ast.find_error tmp in
    let (c, indirect) = Cfg_ast.find_indirect tmp in
    let c = CA.add_edge c indirect error in (* indirect jumps could fail *)
    let c = ref c in
    let state = ref D.State.init in
    let q = Queue.create () in
    let () = Queue.add (entry,None,exp_of_lab (Addr addr)) q in
    let raise_address c a =
      dprintf "raise_address %#Lx" a;
      try c, CA.find_label c (Addr a)
      with Not_found ->
        let (prog, next) = Asmir.asm_addr_to_bap p a in

        (* Hack to remove calls for now *)
        let prog = match List.rev prog with
          | Jmp(_, [StrAttr "call"])::tl ->
            List.map (function
              | Label _ as s -> s
              | s -> Comment(Printf.sprintf "Call statement removed: %s" (Pp.ast_stmt_to_string s), [])) prog
          | _ -> prog
        in
        let (c', edges, bbs, fallthrough) = Cfg_ast.add_prog c prog in
        (* Queue edges *)
        let edges = match fallthrough with
          | Some v -> (v,None,exp_of_lab (Addr next))::edges
          | None -> edges
        in
        List.iter (fun x -> Queue.add x q) edges;
        c', CA.find_label c' (Addr a)
    in
    let add_resolved_edge (s,l,e) c lbl  =
      try
        let c, dst = match lbl with
          | Addr a -> raise_address c a
          | Name s ->
        (* If we can't find a named label, something bad happened *)
            c, CA.find_label c lbl
        in
        CA.add_edge_e c (CA.G.E.create s l dst)
      with Asmir.Memory_error ->
        (* Should this go to error or exit? *)
        wprintf "Raising address %s failed" (Pp.label_to_string lbl);
        let c,error = Cfg_ast.find_error c in
        CA.add_edge_e c (CA.G.E.create s l error)
    in
    while not (Queue.is_empty q) do
      let ((s,l,t) as e) = Queue.pop q in
      dprintf "Looking at edge from %s to %s" (Cfg_ast.v2s s) (Pp.ast_exp_to_string t);
      let resolved_addrs,state' = D.get_succs p !c e !state in
      state := state';
      match resolved_addrs with
      | Addrs addrs ->
        dprintf "%d Addrs" (List.length addrs);
        c := List.fold_left (add_resolved_edge e) !c addrs
      | Error | Exit | Indirect ->
        dprintf "Special";
        let c', dest = match resolved_addrs with
          | Error -> Cfg_ast.find_error !c
          | Exit -> Cfg_ast.find_exit !c
          | Indirect -> Cfg_ast.find_indirect !c
          | _ -> failwith "impossible"
        in
        let c' = CA.add_edge_e c' (CA.G.E.create s l dest) in
        c := c'
    done;

    !c, !state

  let disasm p = disasm_at p (Asmir.get_start_addr p)
end

let recursive_descent =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC) in
  RECURSIVE_DESCENT.disasm

let recursive_descent_at =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC) in
  RECURSIVE_DESCENT.disasm_at

let vsa =
  let module VSA = Make(VSA_SPEC) in
  VSA.disasm

let vsa_at =
  let module VSA = Make(VSA_SPEC) in
  VSA.disasm_at
