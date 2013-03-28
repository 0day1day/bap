(* Steensgard's loop nesting algorithm

   See Steensgaard, B. (1993). Sequentializing Program Dependence
   Graphs for Irreducible Programs (No. MSR-TR-93-14), particularly
   page 5.  (The algorithm is very simple)
*)

module D = Debug.Make(struct let name = "LnfSteensgard" and default=`NoDebug end)
open D
open Lnf

module Make(C: G) =
struct
  (* Get loop information from Steensgard's loop forest *)
  let lnf cfg =
    (* XXX: We should really make a copy here *)
    let module Comp = Graph.Components.Make(C) in
    let module VS = Set.Make(C.V) in
    let f cfg =
      (*
       * We only want to process loops. The scc algorithm will identify
       * isolated nodes, but we only consider those that also have a self loop.
       *)
      let sccs = List.filter (fun scc ->
        match scc with
        | [x] -> C.mem_edge cfg x x
        | _ -> true) (Comp.scc_list cfg) in
      let rec process_scc cfg scc =
        dprintf "process_scc";
        match scc with
        | [] -> failwith "loopinfo_from_steensgard: impossible"
        | [x] -> dprintf "Self loop at %s" (C.v2s x);
          { headers=scc; body=scc; children=[] }
        | _ ->
          let h = Hashtbl.create (List.length scc) in
          List.iter (fun v -> dprintf "scc %s" (C.v2s v); Hashtbl.add h v ()) scc;

          let entry_nodes = C.fold_edges_e (fun e s ->
            if Hashtbl.mem h (C.E.dst e) = true && Hashtbl.mem h (C.E.src e) = false
            then (dprintf "entry %s" (C.v2s (C.E.dst e)); VS.add (C.E.dst e) s)
            else s
          ) cfg VS.empty in

          let closing_edges = C.fold_edges_e (fun e l ->
            if Hashtbl.mem h (C.E.src e) = true && VS.mem (C.E.dst e) entry_nodes = true
            then e::l
            else l
          ) cfg [] in

          dprintf "entry nodes %d closing edges %d" (VS.cardinal entry_nodes) (List.length closing_edges);

          (* Progress check *)
          assert (closing_edges <> []);

          (* Remove closing edges *)
          let cfg = List.iter (C.remove_edge_e cfg) closing_edges; cfg in

          (* SCCs contained in original region *)
          let sccs = List.filter (fun scc ->
            List.for_all (Hashtbl.mem h) scc && match scc with
                                                | [x] -> C.mem_edge cfg x x
                                                | _ -> true
          ) (Comp.scc_list cfg) in

          (* XXX: Probably makes more sense to sort in the steensgard test than here *)
          { headers=List.sort compare (VS.elements entry_nodes)
          ; body=List.sort compare scc
          ; children=List.sort compare (List.map (process_scc cfg) sccs) }

      in
      match sccs with
      | [] -> failwith "loopinfo_from_steensgard: impossible"
      | _ -> List.sort compare (List.map (process_scc cfg) sccs)
    in
    f cfg
end
