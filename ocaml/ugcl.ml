(** Guarded Command Language for Unstructured Programs *)

open Ast_convenience
module BH = Cfg.BH
module CA = Cfg.AST
module D=Debug.Make(struct let name="Ugcl" and default=`Debug end)
open D
open Type

type var = Var.t
type exp = Ast.exp
type label = Type.label

type stmt =
  | Assert of exp
  | Assume of exp
  | Assign of var * exp
  | Seq of stmt * stmt
  | Skip

(** A ugcl program is a regular CFG along with a mapping from BBid to
    Ugcl stmt. *)
type t = (CA.G.t * (CA.G.V.label -> stmt))

let concat_stmt s1 s2 = match s1, s2 with
  | Skip, x -> x
  | x, Skip -> x
  | s1, s2 -> Seq(s1, s2)

let rec string_of_stmt =
  let e2s = Pp.ast_exp_to_string in
  function
    | Assert(e) -> Printf.sprintf "Assert %s" (e2s e)
    | Assume(e) -> Printf.sprintf "Assume %s" (e2s e)
    | Assign(v,e) -> Printf.sprintf "Assign %s, %s" (Pp.var_to_string v) (e2s e)
    | Skip -> "Skip"
    | Seq(s1,s2) -> Printf.sprintf "Seq(%s, %s)" (string_of_stmt s1) (string_of_stmt s2)

(* Begin conversion functions *)

let of_straightline passify stmts =
  let mapf = function
    | Ast.Move(v, e, _) when passify -> Assume(binop EQ (Ast.Var v) e)
    | Ast.Move(v, e, _) -> Assign(v, e)
    | Ast.Assert(e, _) -> Assert(e)
    | Ast.Jmp _
    | Ast.Label _
    | Ast.Comment _ -> Skip
    | Ast.Special _ -> failwith "Special found in straightline"
    | Ast.Halt _ -> failwith "Halt found in straightline"
    | Ast.CJmp _ -> failwith "CJmp found in straightline"
  in
  let gclstmts = List.map mapf stmts in
  let add_stmt s news = concat_stmt s news in
  List.fold_left add_stmt Skip gclstmts

module RevCFG =
struct
  type t = CA.G.t
  module V = CA.G.V
  let iter_vertex = CA.G.iter_vertex
  let iter_succ = CA.G.iter_pred
  let in_degree = CA.G.out_degree
end

module RToposort = Graph.Topological.Make(RevCFG);;

let of_ssacfg ?(passify=false) ?entry ?exit cfg =
  (* We use DSA here because we want edge splitting to happen.  We
     don't necessarily need full DSA. *)
  let cfg = Cfg_ssa.to_astcfg ~dsa:true cfg in
  (* Map BBs to their ugcl *)
  let ugclh = BH.create (CA.G.nb_vertex cfg) in
  (* How do we convert from CFG AST to Unstructured GCL?

     Ast.Move -> Ugcl.Assign
     Ast.Label -> Skip
     Ast.Assert -> Ugcl.Assert
     Ast.Comment -> Skip
     Ast.Special -> error
     Ast.Halt -> error

     Control:
     Ast.Jmp -> Skip, since Jmps are redundant in CFG form
     Ast.CJmp e t1 t2 -> We convert the CJmp to a Skip, but push
       Assume e to the beginning of t1, and Assume (not e) to t2.
       FIXME: Why does edge splitting ensure that there is a unique
       predecessor?
  *)
  let save_gcl bb gcl =
    assert (not (BH.mem ugclh bb));
    BH.add ugclh bb gcl
  in
  let prepend s bb =
    BH.replace ugclh bb (concat_stmt s (BH.find ugclh bb))
  in
  let prepend_assume e bb =
    let t,f = match CA.G.succ_e cfg bb with
    | [f;t] when (CA.G.E.label t) = Some(true) && (CA.G.E.label f) = Some(false) ->
      t, f
    | [t;f] when (CA.G.E.label t) = Some(true) && (CA.G.E.label f) = Some(false) ->
      t, f
    | _ -> failwith "Unable to find successors of cjmp"
    in
    (* Make sure bb is only predecessor of its successors *)
    let e2bbid e = CA.G.V.label (CA.G.E.dst e) in
    assert (match CA.G.pred cfg (CA.G.E.dst t), CA.G.pred cfg (CA.G.E.dst f) with
    | [x], [y] -> true
    | _ -> false);
    prepend (Assume e) (e2bbid t);
    prepend (Assume (unop NOT e)) (e2bbid f)
  in
  let compute_at b =
    let bb_s = CA.get_stmts cfg b in
    (* Debug_snippets.print_ast bb_s; *)
    let ugcl = match List.rev bb_s with
      | [] -> Skip
      | (Ast.Jmp _ | Ast.Halt _)::rest -> of_straightline passify (List.rev rest)
      | Ast.CJmp(e, _, _, _)::rest ->
        (* Prepend Assume e and Assume not e to successors *)
        prepend_assume e b;
        of_straightline passify (List.rev rest)
      | _ -> of_straightline passify bb_s
    in
    save_gcl (CA.G.V.label b) ugcl
  in
  RToposort.iter compute_at cfg;
  (* RToposort.iter *)
  (*   (fun bb -> *)
  (*     dprintf "BB: %s GCL: %s" (Cfg_ast.v2s bb) (string_of_stmt (BH.find ugclh (CA.G.V.label bb))) *)
  (*   ) cfg; *)
  cfg, BH.find ugclh
