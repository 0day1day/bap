open BatPervasives
open OUnit

module G =
struct
  include Graph.Pack.Digraph

  let v2s v = string_of_int (V.label v)
end

module L = Lnf_havlak.Make(G)

open Lnf

let n = G.V.create;;

let v0 = n 0
and va = n 1
and vb = n 2
and vc = n 3
and vd = n 4
and ve = n 5
and vf = n 6
and vg = n 7
and vh = n 8
and vend = n 9

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

let ramalingam_fig2 : test = (v0, [
  (v0, [va; vend]);
  (va, [vb; vc]);
  (vb, [vd]);
  (vc, [ve]);
  (vd, [vb; ve; vend]);
  (ve, [vc; vd; vend]);
  (vend, [])
], [
  {headers=[vb]; body=[vb; vc; vd; ve]; children=[
    {headers=[vd]; body=[vc; vd; ve]; children=[
      {headers=[ve]; body=[vc; ve]; children=[]}
    ]}
  ]}
])

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
  let lnf = L.lnf g v0 in
  let g = build_graph edge_list in
  assert_bool ("Invalid lnf: " ^ (Lnf.string_of_lnf G.v2s lnf))
              (Lnf.validate_lnf lnf);
              assert_equal ~printer:(Lnf.string_of_lnf G.v2s) expected_lnf lnf

let suite = "Steensgard" >:::
  [
    "ramalingam_fig2_test" >:: (fun () -> run_test ramalingam_fig2);
  ]

