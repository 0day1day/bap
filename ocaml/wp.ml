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
	invalid_arg "efficient_wp requires an assignment-free program"
  in
  let q0 = wlp_f p (fun x -> x) in 
  let qpr = wp_t p (fun x -> x) in 
  (fun q -> simp(exp_and qpr (exp_or q0 q)))



let aij_wp ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (p:Gcl.t) =
  let size e = 3 in (* FIXME *)
  let variableify v e =
    if size e > k then
      let x = Var.newvar "x" (Typecheck.infer_ast e) in
      let xe = Var x in
      (BinOp(EQ, xe, e) :: v, xe)
    else
      (v, e)
  in
  let g (v, n, w) =
    let rec g' v ns ws fn fw =
      match (ns,ws) with
      | (n::ns, w::ws) ->
	  let (v,n) = variableify v n in
	  g' v ns ws (exp_and n fn) (exp_or w (exp_and n fw))
      | ([],[]) ->
	  (v, fn, fw)
      | _ -> failwith "n and w are supposed to have the same length"
    in
    match (n,w) with
    | (n::ns, w::ws) -> let (v,n) = variableify v n in g' v ns ws n w
    | ([],[]) -> (v, exp_true, exp_false)
    | _ -> failwith "n and w are supposed to have the same length"
  in
  let rec f ((v,n,w) as vnw) s = match s with
    | Assert e ->
	let (v,e) = if less_duplication then variableify v e else (v,e) in
	(v, e::n, exp_not e :: w)
    | Assume e ->
	(v, e::n, exp_false::w)
    | Seq(a, b) ->
	let vnw' = f vnw a in (* FIXME: do we need tail recursion?*)
	f vnw' b
    | Choice(a, b) ->
	let (v,na,wa) = f (v,[],[]) a in
	let (v,nb,wb) = f (v,[],[]) b in
	let (v,na,wa) = g (v,na,wa) in
	let (v,nb,wb) = g (v,nb,wb) in
	(v, (exp_or na nb)::n, (exp_or wa wb)::w)
    | Skip ->
	vnw
    | Assign _ ->
	invalid_arg "aij_wp requires an assignment-free program"
  in
  let assignments_to_exp v =
    let rec h e = function
      | a::rest -> h (exp_and a e) rest
      | [] -> e
    in
    h exp_true v
  in
  let (v,n,w) = g (f ([],[],[]) p) in
  let v = assignments_to_exp v in
  (fun q -> exp_and v (exp_and (exp_not w) (exp_implies n w)))
