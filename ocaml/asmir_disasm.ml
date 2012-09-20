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
  type ret
  val initial_ret : ret
  val get_succs : Cfg.AST.G.t -> Cfg.AST.G.V.t * Cfg.AST.G.E.label * Ast.exp -> succs
end

module RECURSIVE_DESCENT_SPEC = struct
  type ret = unit
  let initial_ret = ()
  let get_succs g (v,l,e) =
    match List.rev (CA.get_stmts g v), l with
    | CJmp _::_, Some _ ->
      (match lab_of_exp e with
      | Some l -> Addrs [l]
      | None -> Indirect)
    | CJmp _::_, _ -> failwith "error"
    | Jmp _::_, None
    | _::_, None ->
      (match lab_of_exp e with
      | Some l -> Addrs [l]
      | None -> Indirect)
    | _::_, Some _ -> failwith "error"
    | [], _ -> Addrs []

end

module Make(D:DISASM) = struct
  let disasm_at p addr =
    let (tmp, entry) = Cfg_ast.create_entry (CA.empty()) in
    let (tmp, exit) = Cfg_ast.find_exit tmp in
    let (tmp, error) = Cfg_ast.find_error tmp in
    let (c, indirect) = Cfg_ast.find_indirect tmp in
    let c = CA.add_edge c indirect error in (* indirect jumps could fail *)
    let c = ref c in
    let q = Queue.create () in
    let () = Queue.add (entry,None,exp_of_lab (Addr addr)) q in
    let raise_address c a =
      dprintf "raise_address %#Lx" a;
      try c, CA.find_label c (Addr a)
      with Not_found ->
        let (prog, next) = Asmir.asm_addr_to_bap p a in
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
      let c, dst = match lbl with
      | Addr a -> raise_address c a
      | Name s ->
        (* If we can't find a named label, something bad happened *)
        c, CA.find_label c lbl
      in
      CA.add_edge_e c (CA.G.E.create s l dst)
    in
    while not (Queue.is_empty q) do
      let ((s,l,t) as e) = Queue.pop q in
      dprintf "Looking at edge from %s to %s" (Cfg_ast.v2s s) (Pp.ast_exp_to_string t);
      let resolved_addrs = D.get_succs !c e in
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

    !c, D.initial_ret

  let disasm p = disasm_at p (Asmir.get_start_addr p)
end

let recursive_descent =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC) in
  RECURSIVE_DESCENT.disasm

let recursive_descent_at =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC) in
  RECURSIVE_DESCENT.disasm_at
