(** A module to perform coalescing on CFGs *)

open BatListFull
open Cfg

module MakeCoalesce (C: CFG) =
struct
  module G = C.G
  module GS = Set.Make (struct
                         type t = G.V.t
                         let compare = Pervasives.compare
                        end)

 (* The function that does the coalescing. The algorithm is
  * pretty simple:
  *  - While doing a DFS we join two nodes n1 and n2 if
  *    a) n1 has only one successor and
  *    b) n2 has n1 as its only predecessor
  *)
  let coalesce cfg =
   let entry_node = C.find_vertex cfg BB_Entry in
   let visited = ref GS.empty in
   let isspecial v = match G.V.label v with BB _ -> false | _ -> true in
   let add_visited v = visited := GS.add v !visited in
   let rec fold_dfs graph init =
     if GS.mem init !visited then graph
     else
      let worklist, graph =
        (* we start with a node init *)
        let rec immediate_succs acc node =
          match G.succ graph node with
            | [successor] ->
                (match G.pred graph successor with
                   | [_] when not (isspecial successor) ->
                       immediate_succs (successor::acc) successor
                   | _ ->
                       acc)
            | _ ->
                acc
        in
        (* let's get the immediate successor nodes that follow the node.
           In this context, immediate means: n1 -> n2 -> n3 -> (n4|n5)
           should return [n1; n2; n3] *)
        let successors = immediate_succs [] init in
        if successors <> [] then (
          (* Now let's coalesce them cleverly *)
          let init_stmts = C.get_stmts graph init in
          let all_stmts = List.map (C.get_stmts graph) successors in
          let big_stmt_block =
            List.fold_left (fun stmts ith_stmt -> C.join_stmts ith_stmt stmts)
              C.default
              all_stmts
          in
          let big_stmt_block = C.join_stmts init_stmts big_stmt_block in
          (* Replace the contents of init *)
          let graph = C.set_stmts graph init big_stmt_block in
          (* add the edges to the successors *)
          let successors_of_successors_e = G.succ_e graph (List.hd successors) in
          let newsuccessors = G.succ graph (List.hd successors) in
          let add_edge graph edge =
            let newedge = C.G.E.create init (C.G.E.label edge) (C.G.E.dst edge) in
            C.add_edge_e graph newedge
          in
          let graph = List.fold_left add_edge graph successors_of_successors_e in
          (* Remove unused successors *)
          let graph = List.fold_left C.remove_vertex graph successors in
          add_visited init;
          (newsuccessors, graph)
        )
        else (
          let successors = G.succ graph init in
          add_visited init;
          (successors, graph)
        )
     in

     List.fold_left fold_dfs graph worklist
   in
   let graph = fold_dfs cfg entry_node in
   graph
end

module AST_Coalesce = MakeCoalesce(AST)
module SSA_Coalesce = MakeCoalesce(SSA)

