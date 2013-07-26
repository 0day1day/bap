(* Backwards taint analysis on traces *)

open Ast
open Big_int_convenience
open Type

module D = Debug.Make(struct let name = "Exploitable" and default=`NoDebug end)
open D

(* Represent a location, which is either a variable (register) or a
   memory address. *)
module Loc = struct
  type t = V of Var.t | M of Big_int_Z.big_int
  let compare mv1 mv2 =
    match (mv1,mv2) with
    | ((V x),(V y)) -> compare x y
    | ((M x),(M y)) -> Big_int_Z.compare_big_int x y
    | ((V _),(M _)) -> -1
    | ((M _),(V _)) -> 1
  let to_string = function
    | V(v) -> Printf.sprintf "V(%s)" (Pp.var_to_string v)
    | M(i) -> Printf.sprintf "M[%s]" (~% i)
end

module LocSet = Set.Make(Loc)
module VH = Var.VarHash


let print_locset vars =
  Printf.printf "  [+] Cardinality of Set: %d\n" (LocSet.cardinal vars);
  LocSet.iter (fun k -> match k with
  | Loc.V(x) -> Printf.printf "   [-] Var name: %s\n" (Pp.var_to_string x)
  | Loc.M(x) -> Printf.printf "   [-] Addr: %s\n" (Util.big_int_to_hex x)) vars

(* Given an expression e, recursively adds referenced locations to the
 * given set.
 *)
let add_referenced vars e =
  let varvis = object(self)
    inherit Ast_visitor.nop

    method visit_exp e =
      match e with
      | Let (v, e1, e2) ->
        (* Let requires a little special handling *)
        ignore(Ast_visitor.exp_accept self e1);
        vars := LocSet.add (Loc.V v) !vars;
        ignore(Ast_visitor.exp_accept self e2);
        (* XXX: This doesn't handle shadowing Lets properly, e.g., let v
           = 5 in let v = 4 in v *)
        vars := LocSet.remove (Loc.V v) !vars;
        SkipChildren
      | Load (_,Int(addr,_),_,_) ->
        vars := LocSet.add (Loc.M addr) !vars;
        DoChildren
      | Load _ as e ->
        failwith (Printf.sprintf "Found a non-concretized memory read %s, but expected all memory addresses to be concretized" (Pp.ast_exp_to_string e))
      | _ -> DoChildren
    method visit_rvar r =
      if not (LocSet.mem (Loc.V r) !vars) then
        if Typecheck.is_integer_type (Var.typ r) then
          vars := LocSet.add (Loc.V r) !vars;
      DoChildren
  end
  in
  ignore(Ast_visitor.exp_accept varvis e);
  !vars

(* Given an expression e, returns true if the expression is a memory
   write to a location in the interesting set.  *)
let interesting_mem_write vars e =
  let interesting_flag = ref false in
  let mems = ref LocSet.empty in
  let memvis_one = object(self)
  inherit Ast_visitor.nop

  method visit_exp e =
    match e with
    | Store(_,Int(addr,_),value,_,_) ->
      if (LocSet.mem (Loc.M addr) !vars) then (
        mems := LocSet.add (Loc.M addr) !mems;
        interesting_flag := true;
        SkipChildren)
      else
        DoChildren
    | _ -> DoChildren
  end
  in
  ignore(Ast_visitor.exp_accept memvis_one e);
  (!interesting_flag, !mems)

(* Given a trace and an initial location set, finds the starting
   locations that influenced that operand. *)
let backwards_taint stmts locset =
  let rev_stmts = List.rev stmts in
  let vars = ref locset in
  List.iter (fun stmt ->
    (match stmt with
    | Move(l, e, _) ->
      (* If l is interesting, then any location referenced in e is
         interesting too. *)
      if (LocSet.mem (Loc.V l) !vars &&
            Typecheck.is_integer_type (Var.typ l)) then (
        dprintf "interesting: %s" (Pp.ast_stmt_to_string stmt);
        vars := (LocSet.remove (Loc.V l) !vars);
        let old_vars = !vars in
        vars := add_referenced vars e;
        if !vars = old_vars then
          Printf.printf "Leaf instruction: %s\n" (Pp.ast_stmt_to_string stmt)
      ) else (
        (* Alternatively, if there is a write to an interesting memory
           location, then we should also add any referenced
           locations. *)
        let flag, mems = interesting_mem_write vars e in
        if flag then (
          dprintf "interesting: %s" (Pp.ast_stmt_to_string stmt);
          vars := LocSet.diff !vars mems;
          let old_vars = !vars in
          vars := add_referenced vars e;
          if !vars = old_vars then
            Printf.printf "Leaf instruction: %s\n" (Pp.ast_stmt_to_string stmt)
        )
      )
    | _ -> ();
    );

    if LocSet.cardinal !vars = 0 then failwith (Printf.sprintf "Empty taint set at %s" (Pp.ast_stmt_to_string stmt))

  ) rev_stmts;
  if debug() then print_locset !vars;
  !vars

(* Convert expression to a locset *)
let exp_to_locset e =
  let s = ref LocSet.empty in
  let add l = s := LocSet.add l !s in

  let v = object(self)
    inherit Ast_visitor.nop
    method visit_rvar v = add (Loc.V v); SkipChildren
    method visit_exp = function
      | Load(_,Int(addr,_),_,t) when t = reg_8 ->
        add (Loc.M addr);
        DoChildren
      | Load _ ->
        failwith "Memory loads must be 8-bit and have an integer address.  Maybe you need to concretize the trace."
      | _ -> DoChildren
  end in
  ignore(Ast_visitor.exp_accept v e);
  !s

(* Guess faulting locations from trace using heuristics *)
let identify_fault_location t =
  let revt = List.rev t in
  let get_memory s =
    let addrs = ref [] in
    let v = object(self)
      inherit Ast_visitor.nop
      method visit_exp = function
        | Load (_, Int(i,_), _, t)
        | Store (_, Int(i,_), _, _, t) when t = reg_8 ->
          addrs := (Loc.M i) :: !addrs;
          DoChildren
        | Load _ | Store _ ->
          failwith "Memory loads must be 8-bit and have an integer address.  Maybe you need to concretize the trace."
        | _ -> DoChildren
    end
    in
    ignore(Ast_visitor.stmt_accept v s);
    !addrs
  in
  let rec find_fault = function
  (* XXX: We remove Jmps when we concretize... *)
    | Jmp(e, _)::_ when lab_of_exp e = None -> exp_to_locset e
    | s::_ when get_memory s <> [] ->
      List.fold_left (fun s l -> LocSet.add l s) LocSet.empty (get_memory s)
    | _::tl -> find_fault tl
    | [] -> failwith "No faulting location found.  This is probably a bug."
  in
  let loc = find_fault revt in
  if debug() then print_locset loc;
  loc
