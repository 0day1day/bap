(* A module to try out search strategies on symbolic execution *)

module D = Debug.Make(struct let name = "SearchFSE" and default=`Debug end)
open D

open Symbeval

module type STRATEGY =
sig
  type t
  type data
  type initdata
  val start_at : ctx -> initdata -> t
  val pop_next : t -> ((ctx * data) * t) option
  val add_next_states : t -> ctx -> data -> ctx list -> t
end

module MakeSearch(S:STRATEGY) =
struct
  let rec search post predicates q =
    match S.pop_next q with
    | None -> predicates
    | Some ((st,d),q) ->
	let (newstates, predicates) =
	  try (Symbeval.eval st, predicates) with
	  | Halted(v,s) ->
	      let q = symb_to_exp (eval_expr s.delta post) in
	      let pred = Ast.exp_and q s.pred in
	      ([], pred :: predicates)
	  | AssertFailed {pc=pc} ->
	      wprintf "failed assertion at %Ld\n" pc;
	      ([], predicates)  (* try other branches *)
	in
	let q = S.add_next_states q st d newstates in
	search post predicates q

  let eval_ast_program initdata prog post =
    let ctx = Symbeval.build_default_context prog in
    let predicates = search post [] (S.start_at ctx initdata) in
    if debug then dprintf "Explored %d paths." (List.length predicates);
    Util.list_join Ast.exp_or predicates

end

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



module UnboundedBFS = MakeSearch(
  struct
    type t = ctx Q.t
    type data = unit
    type initdata = unit
    let pop_next q = match Q.dequeue_o q with
      | Some(st,q) -> Some((st,()),q)
      | None -> None

    let start_at s () = Q.enqueue Q.empty s
    let add_next_states q st () newstates = Q.enqueue_all q newstates
  end)

let bfs_ast_program p q = UnboundedBFS.eval_ast_program () p q


module MaxdepthBFS = MakeSearch(
  struct
    type t = (ctx * int) Q.t
    type data = int
    type initdata = int

    let pop_next = Q.dequeue_o
    let start_at s i = Q.enqueue Q.empty (s,i)
    let add_next_states q st i newstates =
      if i > 0 then
	List.fold_left (fun q c -> Q.enqueue q (c, i-1)) q newstates
      else
	q
  
  end)

let bfs_maxdepth_ast_program = MaxdepthBFS.eval_ast_program

let rec unbounded_dfs st = 
  List.iter unbounded_dfs (Symbeval.eval st)

let dfs_ast_program p = 
  let ctx = Symbeval.build_default_context p in
  unbounded_dfs ctx


