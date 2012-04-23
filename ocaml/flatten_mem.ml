(** Break complicated memory write statements a series of flat ones of
    form [Store(Var v, ...)]. This makes it easier to execute the
    memory operations sequentially.

    $Id$

    XXX: explode_mem_let should probably make new statements with
    temporary unique variables instead of blowing up the expressions.

    @author ejs
*)

open Ast
module D = Debug.Make(struct let name = "Flatten_mem" and default=`Debug end)
open D

(* Explode Let(x,e,e') when [f e] is true *)
let explode_mem_let f e =
  let h = Hashtbl.create 1000 in
  let v = object(self)
    inherit Ast_visitor.nop

    method push_assign v e =
      Hashtbl.add h v e

    method pop_assign v =
      Hashtbl.remove h v

    method visit_exp = function
      | Var(v) ->
        (* Do we have a value from a Let binding to return? *)
        (try `ChangeTo(Hashtbl.find h v)
         with Not_found ->
           (* Nope, leave it alone *)
           `DoChildren)
      | Let(v,e,e') as bige when f bige ->
        (* Recurse on e, removing any Lets *)
        let e = Ast_visitor.exp_accept self e in
        let () = self#push_assign v e in
        let newe' = Ast_visitor.exp_accept self e' in
        let () = self#pop_assign v in
        (* We don't need to recurse on children because we already did in e' *)
        `ChangeTo newe'
      | e -> `DoChildren
  end
  in
  Ast_visitor.exp_accept v e

let flatten_memexp_rev memvl atts e =
  (* Remove lets that return a memory type *)
  let e = explode_mem_let (fun _ -> true) e in
  let rec flatten_memexp_rev memvl atts = function
    | Var _ as e -> e, []
    | Store(Var _,_,_,_,_) as e -> e, []
    | Store(a,i,v,e,t) ->
      let flatmem, stmts = flatten_memexp_rev memvl atts a in
    (* If a is flat after running stmts, we need to move this Store to
       memvl. *)
      flatmem, Move(memvl, Store(Var memvl,i,v,e,t), atts)::stmts
    | e -> failwith (Printf.sprintf "flatten_memexp: Found non-memory expression (%s) type while flattening" (Pp.ast_exp_to_string e))
  in
  flatten_memexp_rev memvl atts e

(** [flatten_memexp memvl atts e] returns a tuple [(flate, stmts)]
    where [flate] is equivalent to [e] but flat (contains no nested
    Stores), as long as [stmts] are executed immediately before evaluating
    [flate]. *)
let flatten_memexp memvl atts e =
  let e, stmts = flatten_memexp_rev memvl atts e in
  e, List.rev stmts

(** Converts a deep assignment to memory [Move(memv, Store(Store(memv,
    idx1, value1), idx2, value2))] to multiple flat assignments:
    [Move(memv, Store(Var memv, idx1, value1)) :: Move(memv, Store(Var
    memv, idx2, value2)) :: []]. Statements that are not memory writes
    are passed through unchanged.  Lets with memory types are exploded.
*)
let flatten_stores = function
  | Move(memvl, e, att) when Typecheck.is_mem_type (Var.typ memvl) ->
    let e', revstmts = flatten_memexp memvl att e in
    let revstmts = Move(memvl, e', att) :: revstmts in
    List.rev revstmts
  | s -> [s]

(** Converts a Load expression e into a form that can be sequentially
    evaluated concretely.  This means that there are no Let bindings that
    bind a memory state to a variable, because that cannot be implemented
    concretely. *)
let flatten_loads e =
  explode_mem_let
    (function
      | Let(v,_,_) when Typecheck.is_mem_type (Var.typ v) -> true
      | Let _ -> true
      | _ -> false) e
