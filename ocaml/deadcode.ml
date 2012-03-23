(** Dead code elimination for SSA graphs. *)

(* Based off Vine_dataflow.DeadCode *)
open Ssa

module D = Debug.Make(struct let name = "Deadcode" and default=`NoDebug end)
open D
module VH = Var.VarHash
module C = Cfg.SSA
module BH = Hashtbl.Make(C.G.V)

type site = C.G.V.t * Ssa.stmt
	

    
(* return list of lvals defined and used by this stmt *)
(* XXX doesn't match behavior of previous implementation
   w.r.t. Set. Not sure which behavior is correct. *)
(* not performing union on used vars; shouldn't be needed
   for correctness, since double-counting will be symmetrical
   when incrementing and decrementing use-counts *)
let def_uses s =
  let lv, liveout = 
    match s with
    | Move (lv, _, a) when List.mem Type.Liveout a -> ([lv],[lv])
    | Move (lv, _, _) -> ([lv],[])
    | _ -> ([],[])
  in
  let uses = ref [] in
  let vis =  object(self)
    inherit Ssa_visitor.nop
    method visit_rvar v = uses := v :: !uses;
      `DoChildren
  end
  in
  ignore (Ssa_visitor.stmt_accept vis s);
  (lv, !uses, liveout)


(* in SSA, a variable is live at its definition site iff its list of
   uses is not empty. Therefore, calculating live variables is really
   just a matter of calculating whether or not a variable has any
   uses. (p445  ML Tiger book ) *)
(** Performs dead code elimination, returning the new CFG and a bool
    indicating whether anything changed. Any move with the [Liveout]
    attribute will be assumed to be live.
*)
let do_dce ?(globals=[]) graph =
  let (var_to_deps: Ssa.var list VH.t) = VH.create 57 in
  let (var_to_defsite: site VH.t) = VH.create 57 in
  let (usecounts: int ref VH.t) = VH.create 57 in
  let usecount v =
    try VH.find usecounts v
    with Not_found ->
      let r = ref 0 in
      VH.add usecounts v r;
      r
  in
  let deps = VH.find var_to_deps in
  (* get initial mappings *)
  C.G.iter_vertex
    (fun bb ->
       let stmts = C.get_stmts graph bb in
       List.iter
	 (fun s ->
	    let site = (bb, s) in
            let defs, deps, liveout = def_uses s in
            
            (* iterate over defs, updating maps *)
            List.iter
              (fun defd_var ->
                 assert(not (VH.mem var_to_deps defd_var));
                 assert(not (VH.mem var_to_defsite defd_var));
                 VH.add var_to_deps defd_var deps;
                 VH.add var_to_defsite defd_var site;
                 ignore(usecount defd_var); (* Add 0 if needed *)
              )
              defs;
            
            (* update usecounts mapping *)
            List.iter (fun v -> incr (usecount v)) deps;
	    (* increment liveout vars by one, so they are never dead *)
            List.iter (fun v -> incr (usecount v)) liveout;
	 )
         stmts
    )
    graph;

  (* increment use-counts for globals *)
  List.iter (fun g -> incr (usecount g))  globals;

  (* initialize kill list with unused defs *)
  let to_kill = Stack.create() in
  VH.iter (fun v c -> if !c = 0 then Stack.push v to_kill) usecounts;

  (* we'll end up eliminating dead code iff vars_to_kill is non-empty *)
  let has_changed = not(Stack.is_empty to_kill) in
  
  (* iteratively kill stmts defining dead vars, adding newly dead vars
     to var_to_kill, until vars_to_kill is empty *)
  let (dead_sites:(site, unit) Hashtbl.t) = Hashtbl.create 5700 in
  let blocks_to_update = BH.create 5 in
  while not(Stack.is_empty to_kill) do 
    let var_to_kill = Stack.pop to_kill in

    if VH.mem var_to_defsite var_to_kill then (
      (* add defining stmt to kill list *)
      let (bb,stmt) as site_to_kill = VH.find var_to_defsite var_to_kill in
      assert(not (Hashtbl.mem dead_sites site_to_kill));
      Hashtbl.add dead_sites site_to_kill ();
      BH.replace blocks_to_update bb ();

      (* decrement uses of used vars, 
         adding to kill list if uses is now 0 *)
      List.iter
        (fun used_var ->
           let use_count = usecount used_var in
           assert(!use_count > 0);
           decr use_count;
           if !use_count = 0 then Stack.push used_var to_kill;
        )
        (deps var_to_kill)
    ) else
      dprintf "Dead var %s is undefined" (Pp.var_to_string var_to_kill)
  done;
  
  (* go over graph to remove dead sites *)
  dprintf "Deleting %d dead stmts" (Hashtbl.length dead_sites);
  let graph =
    BH.fold
      (fun bb () graph->
	 let stmts = C.get_stmts graph bb in
	 let stmts' =
           List.filter (fun s -> not (Hashtbl.mem dead_sites (bb,s))) stmts
       in
	 C.set_stmts graph bb stmts'
    )
    blocks_to_update
    graph
  in
  (graph, has_changed)

(** Performs aggressive dead code elimination, returning the new CFG
    and a bool indicating whether anything changed. Any move with the
    [Liveout] attribute will be assumed to be live.

    XXX: Is it possible to avoid traversing the whole graph at the end
    to remove dead sites?

    XXX: We do not remove Labels, CJmps, or Jmps, as this requires
    some more thought.

    XXX: Our definition of site might be wrong, since
    non-assignments can be live/dead too, and there is no guarantee
    they are unique in a BB.  On the other hand, if one is dead, and
    the other is identical, they should both be dead (in SSA).
*)
let do_aggressive_dce ?(globals = []) graph =
  let cdg = Depgraphs.CDG_SSA.compute_cdg graph in
  (* Appel book Section 19.5, Aggressive Dead-code Elimination

     "Mark live any statements that
     1. Performs input/output, stores into memory, returns from the
     function, or calls another function that might have side effects
     2. Defines some variable v that is used by another live statement
     3. Is a conditional branch, upon which some other live statement
     is control-dependent.

     Then delete all unmarked statements."

     See deadcode.mli for how we handle #1.
  *)

  let (site_to_deps: (site, site list) Hashtbl.t) = Hashtbl.create 57 in
  let (var_to_defsite: site VH.t) = VH.create 57 in
  let site_is_live, mark_site_as_live =
    let liveh = Hashtbl.create 57 in
    (fun site -> Hashtbl.mem liveh site),
    (fun site -> Hashtbl.replace liveh site ())
  in
  let mark_site_as_initially_live, all_initially_live =
    let initial_live = ref [] in
    (fun site -> initial_live := site :: !initial_live),
    (fun () -> !initial_live)
  in
  (* get initial mappings for var_to_defsite, and mark initial
     statements as live (see deadcode.mli). *)
  C.G.iter_vertex
    (fun bb ->
      let stmts = C.get_stmts graph bb in
      List.iter
	(fun s ->
	  let site = (bb, s) in
          match s with
          | Move(lv, _, a) ->
            assert(not (VH.mem var_to_defsite lv));
            VH.add var_to_defsite lv site;

            (* Mark as liveout if liveout attr is present *)
            if List.mem Type.Liveout a then
              mark_site_as_initially_live site
          | Assert _
          | Comment _
          | Halt _ ->
            mark_site_as_initially_live site
          | _ -> ()
	)
        stmts
    )
    graph;
  (* now get initial mappings for var_to_deps *)
  let control_deps =
    let cdh = Cfg.BH.create 57 in
    (fun bb ->
      try Cfg.BH.find cdh (C.G.V.label bb)
      with Not_found ->
        let d =
          C.G.fold_pred_e
            (fun e l ->
              let src = C.G.E.src e in
              let stmts = C.get_stmts graph src in
              match List.rev stmts with
              | (CJmp _ as stmt)::_ -> (src,stmt) :: l
              | _ when C.G.V.label src = Cfg.BB_Entry -> l (* Everything is dependent on BB_Entry *)
              | _ -> failwith (Printf.sprintf "Expected control-dependency to be a CJmp in %s (%s)!" (Cfg_ssa.v2s bb) (Cfg_ssa.v2s src))
            ) cdg bb []
        in Cfg.BH.add cdh (C.G.V.label bb) d;
        d)
  in
  (* build mapping for var_to_deps *)
  let () = 
    let gen_deps (bb,s) =
      let uses = ref [] in
      let vis = object(self)
        inherit Ssa_visitor.nop
        method visit_rvar v =
          (try uses := VH.find var_to_defsite v :: !uses;
           with Not_found -> ());
          `DoChildren
      end in
      ignore(Ssa_visitor.stmt_accept vis s);
      let deps = !uses @ (control_deps bb) in
      (* dprintf "deps for %s" (Pp.ssa_stmt_to_string s); *)
      (* List.iter (fun (_,s) -> dprintf "%s" (Pp.ssa_stmt_to_string s)) deps; *)
      deps
    in
    C.G.iter_vertex
      (fun bb ->
        let stmts = C.get_stmts graph bb in
        List.iter
	  (fun s ->
	    let site = (bb, s) in
            assert (not (Hashtbl.mem site_to_deps site));
            Hashtbl.add site_to_deps site (gen_deps site)
	  )
          stmts
      )
      graph
  in
  let deps s = try Hashtbl.find site_to_deps s
    with Not_found -> [] in

  let rec do_live worklist = match worklist with
    | [] -> ()
    | site::others when site_is_live site ->
      (* site is already live; ignore it *)
      do_live others
    | (bb,s) as site::others ->
      dprintf "Marking %s as live" (Pp.ssa_stmt_to_string s);
      mark_site_as_live site;
      do_live (worklist@(deps site))
  in
  do_live (all_initially_live ());

  let has_changed = ref false in

  (* Remove dead assignments *)
  let graph = C.G.fold_vertex
    (fun bb graph ->
      let stmts = C.get_stmts graph bb in
      let newstmts = List.filter (function
        | Move _ as s ->
          let alive = site_is_live (bb, s) in
          if not alive then has_changed := true;
          if not alive then dprintf "Dead: %s" (Pp.ssa_stmt_to_string s);
          alive
        | _ -> true (* Don't remove non-assignments for now *)
      ) stmts in
    (* Remove dead conditional jumps

       Consider the following dead CJmp:

                    [CJmp]
                   *      *
                  *        *
             [Dead BB] [Dead BB]
                  *        *
                   *      *
                   [Live BB]

       If CJmp is dead, then there are no live statements that are
       control-dependent on it.  So, we can change the CJmp to
       arbitrarily point to one of the Dead BBs.  Since any live
       statement after the CJmp is not control-dependent on the CJmp, it
       will execute regardless of which branch is taken.
    *)
      let graph = match List.rev newstmts with
        | CJmp (_, t1, _, attrs) as s::others when not (site_is_live (bb, s)) ->
          has_changed := true;
          dprintf "Dead: %s" (Pp.ssa_stmt_to_string s);
          (* Replace CJmp with Jmp to t1 *)
          let newstmts = List.rev
            (Jmp (t1, attrs)::others) in
          let graph = C.set_stmts graph bb newstmts in
          (* Now fix the edges *)
          let truee = List.find (fun e -> C.G.E.label e = Some true) (C.G.succ_e graph bb) in
          let truedst = C.G.E.dst truee in
          let new_edge = C.G.E.create bb None truedst in
          (* Remove all existing edges *)
          let graph = C.G.fold_succ_e
            (fun e graph -> C.remove_edge_e graph e) graph bb graph in
          (* Add the new, unlabeled edge *)
          let graph = C.add_edge_e graph new_edge in
          graph
        | _ -> C.set_stmts graph bb newstmts
      in
      graph

    )
    graph graph in

  graph, !has_changed
