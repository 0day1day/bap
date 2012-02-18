(** Break complicated memory write statements a series of flat ones of
    form [Store(Var v, ...)]. This makes it easier to execute the
    memory operations sequentially.

    $Id$

    @author ejs
*)

open Ast

(* Explode Let(x,e,e') for e' with memory types. *)
let explode_mem_let e =
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
        (try `ChangeToAndDoChildren(Hashtbl.find h v)
         with Not_found ->
           (* Nope, leave it alone *)
           `DoChildren)
      | Let(v,e,e') when Typecheck.is_mem_type (Typecheck.infer_ast ~check:false e') ->
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
  (* Remove lets *)
  let e = explode_mem_let e in
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
let rec flatten_stores = function
  | Move(memvl, e, att) when Typecheck.is_mem_type (Var.typ memvl) ->
    let e', revstmts = flatten_memexp memvl att e in
    let revstmts = Move(memvl, e', att) :: revstmts in
    List.rev revstmts
  | s -> [s]

