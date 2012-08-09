(* Copy propagation

   XXX: Is it possible to implement this for AST and SSA without code
   duplication?
*)

open Ssa
open Type
open Var

module D = Debug.Make(struct let name = "Copy_prop" and default=`NoDebug end)
open D
module VH = Var.VarHash
module VM = Var.VarMap

module CPSpecSSA = struct

  module L = struct
    type et = Middle of Ssa.exp (** One assigned exp *) | Bottom (** Multiple assigned exps *)
    let et_to_string = function
      | Middle e -> Printf.sprintf "Middle %s" (Pp.ssa_exp_to_string e)
      | Bottom -> "Bottom"
    type t = Top | Map of et VM.t
    let top = Top
    let elementmeet x y = match x,y with
      | Bottom, _ -> Bottom
      | _, Bottom -> Bottom
      | x, y when x = y (* For Middle x = Middle x *) -> x
      | x, y -> (* For Middle x = Middle y when x <> y *) Bottom
    let meet x y = match x, y with
      | Top, x -> x
      | x, Top -> x
      | Map x, Map y ->
        Map (VM.fold
          (fun k v newmap ->
              (* Take the meet of each element. If not found, use v,
                 since elementmeet v v = v *)
              let v' = try VM.find k newmap with Not_found -> v in
              VM.add k (elementmeet v v') newmap
          ) x y)
    let equal x y = match x, y with
      | Top, Top -> true
      | Map x, Map y -> VM.equal (=) x y
      | _, _ -> false
  end
  module G = Cfg.SSA.G

  let node_transfer_function g bb l =
    let l = match l with | L.Map m -> m | L.Top -> failwith "Expected Map, not Top" in
    let stmts = Cfg.SSA.get_stmts g bb in
    L.Map (List.fold_left
      (fun l stmt -> match stmt with
      | Move(v, Phi _, _) ->
        VM.add v L.Bottom l
      | Move(v,e,_) ->
        (* dprintf "ignoring %s" (Pp.ssa_stmt_to_string s); *)
        VM.add v (L.Middle e) l
      | _ -> l
      ) l stmts)

  let edge_transfer_function g e l = l

  let s0 _ = Cfg.SSA.G.V.create Cfg.BB_Entry
  let init _ = L.Map VM.empty
  let dir = GraphDataflow.Forward
end

module CPSpecAST = struct

  module L = struct
    type et = Middle of Ast.exp (** One assigned exp *) | Bottom (** Multiple assigned exps *)
    let et_to_string = function
      | Middle e -> Printf.sprintf "Middle %s" (Pp.ast_exp_to_string e)
      | Bottom -> "Bottom"
    type t = Top | Map of et VM.t
    let top = Top
    let elementmeet x y = match x,y with
      | Bottom, _ -> Bottom
      | _, Bottom -> Bottom
      | x, y when x = y (* For Middle x = Middle x *) -> x
      | x, y -> (* For Middle x = Middle y when x <> y *) Bottom
    let meet x y = match x, y with
      | Top, x -> x
      | x, Top -> x
      | Map x, Map y ->
        Map (VM.fold
          (fun k v newmap ->
              (* Take the meet of each element. If not found, use v,
                 since elementmeet v v = v *)
              let v' = try VM.find k newmap with Not_found -> v in
              VM.add k (elementmeet v v') newmap
          ) x y)
    let equal x y = match x, y with
      | Top, Top -> true
      | Map x, Map y -> VM.equal (=) x y
      | _, _ -> false
  end
  module G = Cfg.AST.G

  let node_transfer_function g bb l =
    let l = match l with | L.Map m -> m | L.Top -> failwith "Expected Map, not Top" in
    let stmts = Cfg.AST.get_stmts g bb in
    L.Map (List.fold_left
      (fun l stmt -> match stmt with
      | Ast.Move(v,e,_) ->
        (* dprintf "ignoring %s" (Pp.ast_stmt_to_string s); *)
        VM.add v (L.Middle e) l
      | _ -> l
      ) l stmts)

  let edge_transfer_function g e l = l

  let s0 _ = Cfg.AST.G.V.create Cfg.BB_Entry
  let init _ = L.Map VM.empty
  let dir = GraphDataflow.Forward
end

module CPSSA = GraphDataflow.Make(CPSpecSSA)
module CPAST = GraphDataflow.Make(CPSpecAST)

let copyprop_ssa g =

  let _, dfout = CPSSA.worklist_iterate g in
  (* copy propagation is used during iterative indirect jump
     resolving.  Unfortunately, this means there may not be a path to
     exit from the indirect jump.  Thus, we should take results from all
     nodes, not just BB_Exit *)

  let map_union m oldmap =
    VM.fold
      (fun k v newmap ->
        let old = try VM.find k newmap with Not_found -> v in
        let meet = CPSpecSSA.L.elementmeet v old in
        VM.add k meet newmap
      ) m oldmap
  in
  let unionmap = Cfg.SSA.G.fold_vertex
    (fun v m ->
      (* let newm = dfout v in *)
      let newm = match dfout v with
        | CPSpecSSA.L.Map m -> m
        | _ -> failwith "Expected a map"
      in
      map_union newm m) g VM.empty
  in

  let rec propagate l v =
    let vis = object(self)
      inherit Ssa_visitor.nop
      method visit_exp = function
        | Var v ->
          (try
            match VM.find v l with
            | CPSpecSSA.L.Middle e -> ChangeToAndDoChildren e
            | _ -> SkipChildren
          with Not_found -> SkipChildren)
        | _ -> DoChildren
    end in
    Ssa_visitor.exp_accept vis v
  in
  VM.fold (fun k v newmap ->
    (match v with
    | CPSpecSSA.L.Middle x ->
      let ssae = propagate unionmap x in
      (* dprintf "%s maps to %s" (Pp.var_to_string k) (Pp.ssa_exp_to_string aste); *)
      VM.add k ssae newmap
    | _ ->
      newmap)
  ) unionmap VM.empty
