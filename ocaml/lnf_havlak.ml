module D = Debug.Make(struct let name = "LnfHavlak" and default=`NoDebug end)
open D
open Lnf

module Make(C: G) =
struct
  (* Get loop information from Havlakcs loop forest *)
  let lnf cfg v0 =
    let module Comp = Graph.Components.Make(C) in
    let module VS = Set.Make(C.V) in
    let module Dfs = Graph.Traverse.Dfs(C) in 
    let visited = ref [] in 
    let () = Dfs.postfix_component (fun v -> visited := v::!visited) cfg v0 in
    let dfs_order = !visited in 
    let find_header scc =
      let rec find_first find_these in_here =
        match find_these with 
        | [] -> raise Not_found 
        | x::rest -> if List.mem x in_here then x else find_first rest in_here in
      find_first dfs_order scc in 

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
        | [] -> failwith "loopinfo_from_havlak: impossible"
        | [x] -> dprintf "Self loop at %s" (C.v2s x);
          { headers=scc; body=scc; children=[] }
        | _ ->
          let h = Hashtbl.create (List.length scc) in
          List.iter (fun v -> dprintf "scc %s" (C.v2s v); Hashtbl.add h v ()) scc;
          let cfg = cfg in

          let entry_nodes = VS.add (find_header scc) VS.empty in 

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

          { headers=List.sort compare (VS.elements entry_nodes)
          ; body=List.sort compare scc
          ; children=List.sort compare (List.map (process_scc cfg) sccs) }

      in
      match sccs with
      | [] -> failwith "loopinfo_from_havlak: impossible"
      | _ -> List.sort compare (List.map (process_scc cfg) sccs)
    in
    f cfg
end
