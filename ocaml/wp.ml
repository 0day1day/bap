(** Weakest Precondition.

    These functions calculate the weakest precondition wp(p,q) for
    postcondition q and gcl program p.
*)

open Ast
open Type
open Gcl

module D = Debug.Make(struct let name = "WP" and default=`Debug end)
open D



(** wp(p,q), applying simp to simplify each intermediate expression
    during the calculation.  See "A Discipline of Programming" by
    Dijkstra, or CMU-CS-08-159 (Brumley's thesis), chapter 3.2.

    @param simp is an optional expression simplifier.

    @param p is the program
    
    @param q is the post-condition.
*)
let wp ?(simp=Util.id) (p:Gcl.t) (q:exp) : exp =
  (*  We use CPS to avoid stack overflows *)
  let rec wp q s k =
    match s with
    | Skip -> k q
    | Assume e ->
	k (simp(exp_implies e q))
    | Choice(s1, s2) ->
	wp q s1 (fun x -> wp q s2 (fun y -> k(simp(exp_and x y ))))
    | Seq(s1, s2) ->
	wp q s2 (fun x -> wp x s1 k)
    | Assign(t, e) ->
	k(simp(Let(t, e, q)))
    | Assert e -> 
	k (simp(exp_and e q))
  in
  wp q p  (fun x->x)




(** the efficient weakest precondition wp(p,q):Q where Q is guaranteed
    to be linear in the size of p.  This version expects p to be
    assignment-free, e.g., to be an SSA acyclic program. See
    CMU-CS-08-159.pdf (Brumley's thesis), chapter 3.3.  

    @param simp is an expression simplifier. You can pass in fun x->x
    if you want no simplification.  

    @param p is the program
    
    @param q is the post-condition.
*)
let efficient_wp ?(simp=Util.id) (p:Gcl.t) =
  let wlp_f_ctx = Hashtbl.create 113 in 
  let rec wlp_f s k =
    try k (Hashtbl.find wlp_f_ctx s)
    with Not_found ->
      let remember q = Hashtbl.add wlp_f_ctx s q; k q in
      match s with
      | Skip -> 
	  remember exp_false
      | Assume e
      | Assert e -> 
	  remember (exp_not e)
      | Choice(s1, s2) ->
	  wlp_f s1 (fun q1 -> wlp_f s2 
		      (fun q2 -> 
			 let q = simp(exp_and q1 q2 ) in
			 remember q
		      ))
      | Seq(s1, s2) ->
	  wlp_f s1 (fun q1 -> wlp_f s2 
		      (fun q2 -> 
			 let q = simp(exp_or q1 q2 ) in
			 remember q
		      ))
      | Assign _ -> 
	  raise (Invalid_argument("efficient_wp requires an assignment-free program"))
  in
  let rec wp_t s k : exp = 
    match s with
    | Skip
    | Assume _ ->
	k exp_true
    | Assert e ->
	k e
    | Choice(s1,s2) ->
	wp_t s1 (fun q1 -> wp_t s2 (fun q2 -> k(simp(exp_and q1 q2))))
(*	  (* by the book^Wthesis *)
    | Seq(s1, s2) ->
	wp_t s2 (fun q1 -> wp_t s1 
		   (fun q2 -> wlp_f s1
		      (fun q3 -> k(simp(exp_and q1 (exp_or q2 q3)))) ))
*)
	  (* by my own derivation *)
    | Seq(s1, s2) ->
	wp_t s1 (fun q1 -> wp_t s2
		   (fun q2 -> wlp_f s1
		      (fun q3 -> k(simp(exp_and q1 (exp_or q2 q3)))) ))
    | Assign _ -> 
	raise (Invalid_argument("efficient_wp requires an assignment-free program"))
  in
  let q0 = wlp_f p (fun x -> x) in 
  let qpr = wp_t p (fun x -> x) in 
  (fun q -> simp(exp_and qpr (exp_or q0 q)))

