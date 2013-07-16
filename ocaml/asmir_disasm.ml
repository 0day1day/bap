open Ast
open Ast_convenience
open Big_int_convenience
module CA = Cfg.AST
module CS = Cfg.SSA
module D = Debug.Make(struct let name = "Asmir_disasm" and default=`NoDebug end)
open D
open Type

(* XXX: Handle conditional function calls *)
(* VSA dataflow reuse *)
(* Call/ret behavior *)
(* Reprocess indirect jumps *)

let no_indirect = true

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
  val get_succs : Asmir.asmprogram -> Cfg.AST.G.t -> Cfg.AST.G.V.t * Cfg.AST.G.E.label * Ast.exp -> State.t -> succs * State.t
  (** Function that returns the successors of a node *)

  val fixpoint : bool
  (** Should [get_succs] be called until a fixpoint is reached? *)
end

module RECURSIVE_DESCENT_SPEC = struct
  module State = struct
    type t = unit
    let init = ()
  end
  let get_succs_int ?(no_indirect=no_indirect) _asmp g (v,l,e) () =
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
        | None ->
          if no_indirect
          then failwith "Indirect jump encountered by recursive descent"
          else Indirect)
      | _::_, Some _ -> failwith "error"
      | [], _ -> Addrs []
    in
    o, ()
  let get_succs asmp g e () = get_succs_int asmp g e ()

  let fixpoint = false
end

module VSA_SPEC = struct
  module State = struct
    type t = unit
    let init = ()
  end

  let get_succs asmp g (v,l,e) () =
    match RECURSIVE_DESCENT_SPEC.get_succs_int ~no_indirect:false asmp g (v,l,e) () with
    | Indirect, () ->
      (* Do VSA stuff *)
      dprintf "Resolving %s with VSA" (Pp.ast_exp_to_string e);
      if l <> None then failwith "VSA-enabled lifting currently assumes that conditional jumps are not indirect";
      let cfg = Hacks.ast_exit_indirect (CA.copy g) in
      let cfg = Prune_unreachable.prune_unreachable_ast cfg in
      let cfg = Hacks.add_sink_exit cfg in
      let {Cfg_ssa.cfg=ssacfg; to_ssaexp} = Cfg_ssa.trans_cfg ~tac:false cfg in
      let ssacfg = Ssa_cond_simplify.simplifycond_ssa ssacfg in
      (* let ssacfg = Coalesce.coalesce_ssa ~nocoalesce:[ssav] ssacfg in *)
      (* Cfg_pp.SsaStmtsDot.output_graph (open_out "vsa.dot") ssacfg; *)
      let ssae = to_ssaexp (Vsa_ast.last_loc cfg v) e in
      dprintf "ssae: %s" (Pp.ssa_exp_to_string ssae);
      let ssav = CS.find_vertex ssacfg (CA.G.V.label v) in
      let ssaloc = Vsa_ssa.last_loc ssacfg ssav in
      dprintf "Starting VSA now";
      let df_in, df_out = Vsa_ssa.vsa ~nmeets:50 ~opts:{Vsa_ssa.AlmostVSA.DFP.O.initial_mem=Asmir.get_readable_mem_contents_list asmp} ssacfg in

      let add_indirect () =
        if debug () then (
          Cfg.SSA.G.iter_vertex (fun v ->
            Printf.eprintf "VSA @%s" (Cfg_ssa.v2s ssav);
            Vsa_ssa.AbsEnv.pp prerr_string (BatOption.get (df_out (Vsa_ssa.last_loc ssacfg v)));
            dprintf "\n\n"
          ) ssacfg
        );
        if no_indirect
        then failwith "VSA disassembly failed to resolve an indirect jump to a specific concrete set"
        else Indirect, ()
      in

      let fallback () =
        let vs = Vsa_ssa.AlmostVSA.DFP.exp2vs (BatOption.get (df_out (Vsa_ssa.last_loc ssacfg ssav))) ssae in
        dprintf "VSA resolved %s to %s" (Pp.ast_exp_to_string e) (Vsa_ssa.VS.to_string vs);
        (match Vsa_ssa.VS.concrete ~max:1024 vs with
        | Some x -> dprintf "VSA finished"; Addrs (List.map (fun a -> Addr a) x), ()
        | None -> wprintf "VSA disassembly failed to resolve %s/%s to a specific concrete set" (Pp.ast_exp_to_string e) (Vsa_ssa.VS.to_string vs);
          add_indirect ())
      in

      (* Value sets are very good at representing the addresses that
         point to the elements in a jump table.  For instance,
         jump_table + 4*index is a value set.  However, there is no
         guarantee that the address stored *in* the jump table,
         i.e. M[jump_table + 4*index] are aligned.  To ameliaorate
         this fact, we can apply copy propagation to the jump
         expression, to see if we get something like [Jump Load(e)].
         If we do, we can enumerate the addresses in [e], and then put
         the values [M[e]] in a set.  This is basically what VSA will
         do, except that it will also convert M[e] to a value set
         before it returns, which introduced imprecision. *)

      let special_memory m e endian t =
        (* The variable copy propagates to a memory load.
           Perfect.  This is looking like a jump table lookup. *)
        let l = BatOption.get (df_in ssaloc) in
        let exp2vs = Vsa_ssa.AlmostVSA.DFP.exp2vs l in
        let vs = exp2vs e in
        dprintf "VSA resolved memory index %s to %s" (Pp.ssa_exp_to_string e) (Vsa_ssa.VS.to_string (Vsa_ssa.AlmostVSA.DFP.exp2vs l e));
        (match Vsa_ssa.VS.concrete ~max:1024 vs with
        | Some l -> dprintf "VSA finished";
               (* We got some concrete values for the index.  Now
                  let's do an abstract load for each index, and try to
                  get concrete numbers for that. *)
          let reads = List.map (fun a ->
            let a = bi64 a in
            let vs = exp2vs (Ssa.Load(m, Ssa.Int(a, Typecheck.infer_ssa e), endian, t)) in
            let conc = Vsa_ssa.VS.concrete ~max:1024 vs in
            match conc with
            | Some l -> l
            | None -> failwith (Printf.sprintf "Unable to read from jump table at %s" (~% a))) l in
          Addrs (List.map (fun a -> Addr a) (List.flatten reads)), ()
        | None -> wprintf "VSA disassembly failed to resolve %s/%s to a specific concrete set" (Pp.ssa_exp_to_string e) (Vsa_ssa.VS.to_string vs);
          add_indirect ())
      in

      (match ssae with
      | Ssa.Var var ->
        (* Ok, we matched a variable.  Let's see what copy propagation
           says. *)
        let stop_before = function
          | Ssa.Store _ -> true
          | _ -> false
        in
        let stop_after = function
          | Ssa.Load _ -> true
          | _ -> false
        in
        let m, _ = Copy_prop.copyprop_ssa ~stop_before ~stop_after ssacfg in
        (try
           let e' = Var.VarMap.find var m in
           match e' with
           | Ssa.Load(m, e, endian, t) ->
             special_memory m e endian t
           | _ -> fallback ()
         with Not_found | BatOption.No_value -> fallback ())
      | Ssa.Load(m, e, endian, t) ->
        special_memory m e endian t
      | _ -> fallback ())
    (* Rely on recursive descent for easy stuff *)
    | o, () -> o, ()

  let fixpoint = true
end

module Make(D:DISASM)(F:FUNCID) = struct
  let disasm_at p addr =
    let (tmp, entry) = Cfg_ast.create_entry (CA.empty()) in
    let (tmp, exit) = Cfg_ast.find_exit tmp in
    let (tmp, error) = Cfg_ast.find_error tmp in
    let (c, indirect) = Cfg_ast.find_indirect tmp in
    let c = CA.add_edge c indirect error in (* indirect jumps could fail *)

    (* Store the unresolved edges outgoing from each address.  This is
       used to implement propagate_edge. *)
    let edgeh = Hashtbl.create 1000 in

    let iteration init_worklist (c,state) =
      let c = ref c in
      let state = ref state in
      let q = Worklist.create () in
      (* bbs of lifted insruction, unresolved outgoing edges from lifted instruction, successor address *)
      let () = Worklist.add_list init_worklist q in
      let raise_address c a =
        dprintf "raise_address %#Lx" a;
        try c, CA.find_label c (Addr a)
        with Not_found ->
          let (prog, next) = Asmir.asm_addr_to_bap p a in
          if Asmir.bap_fully_modeled prog = false then raise Asmir.Memory_error;

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

          (* Worklist edges *)
          let edges = match fallthrough with
            | Some v -> (v,None,exp_of_lab (Addr next))::edges
            | None -> edges
          in

          (* Rewrite any calls from this address *)
          let c', edges = match F.find_calls c' bbs edges F.State.init with
            | [], _ -> c', edges
            | call_edges, _ ->
              (* We found edges corresponding to calls *)
              let others = Util.list_difference edges call_edges in
              let fallthrough = List.map (fun (s,l,e) -> (s,l, exp_of_lab (Addr next))) call_edges in
              let dumb_translate_call cfg (s,l,e) =
                let revstmts = match List.rev (CA.get_stmts cfg s) with
                  | CJmp _::_ -> failwith "Conditional function calls are not implemented"
                  | Jmp _::tl as stmts -> List.map (function
                      | Label _ as s -> s
                      | Jmp(e, _) as s ->
                        Comment(Printf.sprintf "Function call removed: %s" (Pp.ast_stmt_to_string s), [NamedStrAttr("calltarget", Pp.ast_exp_to_string e)])
                      | s ->
                        Comment(Printf.sprintf "Function call removed: %s" (Pp.ast_stmt_to_string s), [])) stmts
                  | _ -> failwith "Unable to rewrite function call"
                in
                CA.set_stmts cfg s (List.rev revstmts)
              in
              let c' = List.fold_left dumb_translate_call c' call_edges in
              c', BatList.append others fallthrough
          in

          Hashtbl.replace edgeh a (bbs, edges, next);
          Worklist.add (bbs, edges, next) q;
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
              Some(true (* XXX: This is meaningless! *), binop EQ e (Int(bi64 a, Typecheck.infer_ast e)))
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
      (* Side effects: Adds to worklist *)
      let process_edge (c,state) ((s,l,t) as e) =
        dprintf "Looking at edge from %s to %s" (Cfg_ast.v2s s) (Pp.ast_exp_to_string t);
        let resolved_addrs, state' = D.get_succs p c e state in
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
      while not (Worklist.is_empty q) do
        let (bbs, unresolved_edges, succ) = Worklist.pop q in
        let c',state' = List.fold_left process_edge (!c,!state) unresolved_edges in
        c := c';
        state := state'
      done;

      !c, !state
    in
    let c, state = iteration [(entry::[], (entry,None,exp_of_lab (Addr addr))::[], addr)] (c,D.State.init) in

    if D.fixpoint = false then c, state
    else
      let continue = ref true in
      let c = ref c in
      let state = ref state in
      let iter = ref 0 in
      while !continue && !iter < 5 do
        dprintf "Running cfg recovery until fixpoint: iteration %d" !iter;
        let origc = !c in
        let worklist = Hashtbl.fold (fun k v a -> v::a) edgeh [] in
        let c',state' = iteration worklist (!c,!state) in
        c := c';
        state := state';
        continue := origc <> !c;
        incr iter;
      done;

      !c, !state

  let disasm p = disasm_at p (Asmir.get_start_addr p)
end

let recursive_descent =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC)(FUNCFINDER_DUMB) in
  RECURSIVE_DESCENT.disasm
let recursive_descent a = fst(recursive_descent a)

let recursive_descent_at =
  let module RECURSIVE_DESCENT = Make(RECURSIVE_DESCENT_SPEC)(FUNCFINDER_DUMB) in
  RECURSIVE_DESCENT.disasm_at
let recursive_descent_at a b = fst(recursive_descent_at a b)

let vsa =
  let module VSA = Make(VSA_SPEC)(FUNCFINDER_DUMB) in
  VSA.disasm
let vsa a = fst(vsa a)

let vsa_at =
  let module VSA = Make(VSA_SPEC)(FUNCFINDER_DUMB) in
  VSA.disasm_at
let vsa_at a b = fst(vsa_at a b)
