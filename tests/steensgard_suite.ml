open BatPervasives
open OUnit

module G =
struct
  include Graph.Pack.Digraph

  let v2s v = string_of_int (V.label v)
end

module L = Lnf_steensgard.Make(G)

open Lnf

let n = G.V.create;;

let v0 = n 1
and va = n 2
and vb = n 3
and vc = n 4
and vd = n 5
and ve = n 6
and vf = n 7
and vg = n 8
and vh = n 9
and vend = n 10

(* 
 * TODO(awreece) Is there a better way?
 *
 * The lnf test spec is of the form (start_vertex, edge_list, expected_lnf)
 *
 * edge_list is the specification for the initial graph, and is a list of every
 * vertex and all outgoing edges. It is of the form (src, dest_list) where
 * dest_list is a list of vertices.
 *)

type vertex_spec = G.V.t * (G.V.t list)
type test = G.V.t * (vertex_spec list) * G.V.t lnf

let steensgard_fig2 : test = (v0, [
  (v0, [va; vend]);
  (va, [vb; vc]);
  (vb, [vc]);
  (vc, [vd]);
  (vd, [vb]);
  (vend, [])
], [{headers=[vb; vc]; body=[vb; vc; vd]; children=[]}])

let steensgard_fig3 : test = (v0, [
  (v0, [va; vend]);
  (va, [vb]);
  (vb, [vc]);
  (vc, [vd]);
  (vd, [vb; vc]);
  (vend, [])
], [{headers=[vb]; body=[vb; vc; vd]; children=[
  {headers=[vc]; body=[vc; vd]; children=[]}
]}])

(*
 * build_graph: vertex_spec list -> G.t
 *
 * build_graph spec Constructs the input graph from the provided specification.
 *)
let build_graph vertex_spec_list = 
  let g = G.create () in
  let rec add_edges (src,dl) = match dl with
    | [] -> ()
    | dst::vl -> (G.add_edge g src dst; add_edges (src,vl)) in
  List.iter add_edges vertex_spec_list; g;;

(*
 * run_test: test
 * run_test test_spec runs the given test.
 *)
let run_test (v0,edge_list,expected_lnf) =
  let g = build_graph edge_list in
  let lnf = L.lnf g in
  assert_bool ("Invalid lnf: " ^ (Lnf.string_of_lnf G.v2s lnf))
              (Lnf.validate_lnf lnf);
  assert_equal ~printer:(Lnf.string_of_lnf G.v2s) expected_lnf lnf

let suite = "Steensgard" >:::
  [
    "steensgard_fig2_test" >:: (fun () -> run_test steensgard_fig2);
    "steensgard_fig3_test" >:: (fun () -> run_test steensgard_fig3);
  ]

