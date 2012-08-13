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
  module CFG = Cfg.SSA

  let stmt_transfer_function g stmt l =
    let l = match l with | L.Map m -> m | L.Top -> failwith "Expected Map, not Top" in
    L.Map (match stmt with
    | Move(v, Phi _, _) ->
      VM.add v L.Bottom l
    | Move(v,e,_) ->
        (* dprintf "ignoring %s" (Pp.ssa_stmt_to_string s); *)
      (* let newv = try L.elementmeet (VM.find v l) (L.Middle e) with Not_found -> L.Middle e in *)
      VM.add v (L.Middle e) l
    | _ -> l)

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
  module CFG = Cfg.AST

  let stmt_transfer_function g stmt l =
    let l = match l with | L.Map m -> m | L.Top -> failwith "Expected Map, not Top" in
    L.Map (match stmt with
    | Ast.Move(v,e,_) as _s ->
      (* dprintf "seeing %s" (Pp.ast_stmt_to_string s); *)
      (* let newv = try L.elementmeet (VM.find v l) (L.Middle e) with Not_found -> L.Middle e in *)
      VM.add v (L.Middle e) l
    | _ -> l)

  let edge_transfer_function g e l = l

  let s0 _ = Cfg.AST.G.V.create Cfg.BB_Entry
  let init _ = L.Map VM.empty
  let dir = GraphDataflow.Forward
end

module CPSSA = CfgDataflow.Make(CPSpecSSA)
module CPAST = CfgDataflow.Make(CPSpecAST)

let copyprop_ssa g =

  let _, dfout = CPSSA.worklist_iterate g in
  let rec propagate l v =
    let vis = object(self)
      inherit Ssa_visitor.nop
      method visit_exp = function
        | Ssa.Var v ->
          (try
             match VM.find v l with
             | CPSpecSSA.L.Middle e ->
               ChangeToAndDoChildren e
             | _ -> SkipChildren
           with Not_found -> SkipChildren)
        | _ -> DoChildren
    end in
    Ssa_visitor.exp_accept vis v
  in

  let l = dfout (Cfg.SSA.G.V.create Cfg.BB_Error) in
  let l = match l with
    | CPSpecSSA.L.Map m -> m
    | _ -> failwith "Expected to find a map: BB_Exit probably unreachable"
  in
  VM.fold (fun k v newmap ->
    (match v with
    | CPSpecSSA.L.Middle x ->
      let ssae = propagate l x in
      (* dprintf "%s maps to %s" (Pp.var_to_string k) (Pp.ssa_exp_to_string ssae); *)
      VM.add k ssae newmap
    | _ ->
      newmap)
  ) l VM.empty, propagate l

let copyprop_ast g =

  let dfin, _ = CPAST.worklist_iterate_stmt g in
  let rec propagate l v =
    let vis = object(self)
      inherit Ast_visitor.nop
      method visit_exp = function
        | Ast.Var v ->
          (try
             match VM.find v l with
             | CPSpecAST.L.Middle e ->
               ChangeToAndDoChildren e
             | _ -> SkipChildren
           with Not_found -> SkipChildren)
        | _ -> DoChildren
    end in
    Ast_visitor.exp_accept vis v
  in

  (fun (bb,n) ->
  let l = dfin (bb,n) in
  let l = match l with
    | CPSpecAST.L.Map m -> m
    | _ -> failwith "Expected to find a map: BB_Exit probably unreachable"
  in
  VM.fold (fun k v newmap ->
    (match v with
    | CPSpecAST.L.Middle x ->
      let aste = propagate l x in
      (* dprintf "%s maps to %s" (Pp.var_to_string k) (Pp.ast_exp_to_string aste); *)
      VM.add k aste newmap
    | _ ->
      newmap)
  ) l VM.empty, propagate l)
