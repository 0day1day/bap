(* A module to try out search strategies on symbolic execution *)

open Symbeval

(** A purely functional queue with amortised constant time enqueue and dequeue. *)
module Q = struct
  (* Maybe put this in Util as FQueue? *)
  type 'a t = 'a list * 'a list

  let empty = ([],[])

  let enqueue (a,b) v = (a, v::b)

  let enqueue_all (a,b) l =
    (a, List.rev_append l b)

  let dequeue = function
    | (v::a, b) -> (v, (a,b))
    | ([], b) ->
	match List.rev b with
	| v::a -> (v, (a,[]))
	| [] -> raise Queue.Empty

  let dequeue_o q =
    try Some(dequeue q) with Queue.Empty -> None
end


let rec unbounded_dfs st = 
  List.iter unbounded_dfs (Symbolic.eval st)

let rec bfs post predicates q =
  match Q.dequeue_o q with
  | None -> predicates
  | Some (st,q) ->
      let (newstates, predicates) =
	try (Symbolic.eval st, predicates) with
	| Halted(v,s) ->
	    let q = symb_to_exp (eval_expr s.delta post) in
	    let pred = Ast.exp_and q s.pred in
	    ([], pred :: predicates)
	| AssertFailed _ ->
	    ([], predicates)  (* try other branches *)
      in
      bfs post predicates (Q.enqueue_all q newstates)

let dfs_ast_program p = 
  let ctx = Symbolic.build_default_context p in
  unbounded_dfs ctx

let bfs_ast_program p q =
  let ctx = Symbolic.build_default_context p in
  let predicates = bfs q [] (Q.enqueue Q.empty ctx) in
  Util.list_join Ast.exp_or predicates
