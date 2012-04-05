(** Functions for computing the weakest preconditions (WPs) of
    programs.

    Given a program [p] and a postcondition [q], the weakest
    precondition [wp(p,q)] describes all input states that will cause
    [p] to terminate in a state satisfying [q]. Generally, the
    precondition is solved with an SMT solver.
*)

(** [wp p q] uses Dijkstra's classic WP algorithm to compute
    [wp(p,q)], applying simp to simplify each intermediate expression
    during the calculation.  See "A Discipline of Programming" by
    Dijkstra, or CMU-CS-08-159 (Brumley's thesis), chapter 3.2.  This
    algorithm may produce formulas exponential in the size of the
    program.

    @param simp is an optional expression simplifier.

    @param p is the program

    @param q is the post-condition.
*)
val wp : ?simp:(Ast.exp -> Ast.exp) -> Gcl.t -> Ast.exp -> Ast.exp
val uwp : ?simp:(Ast.exp -> Ast.exp) -> Ugcl.t -> Ast.exp -> Ast.exp
(** Same as {!wp}, but avoids converting the program to GCL. See
    "Weakest-Precondition of Unstructured Programs" by Barnett for the
    general technique. *)
val efficient_wp : ?simp:(Ast.exp -> Ast.exp) -> Gcl.t -> Ast.exp -> Ast.exp
(** [efficient_wp p q] computes [wp(p,q)] using an algorithm that
    guarantees the resulting precondition will be linear in the size
    of p.  [efficient_wp] expects p to be assignment-free, e.g., to be
    an SSA acyclic program. See CMU-CS-08-159.pdf (Brumley's thesis),
    chapter 3.3.

    @param simp is an expression simplifier. You can pass in fun x->x
    if you want no simplification.

    @param p is the program

    @param q is the post-condition.
*)
val efficient_uwp :
  ?simp:(Ast.exp -> Ast.exp) -> Ugcl.t -> Ast.exp -> Ast.exp
(** Same as {!efficient_wp} but does not convert to GCL. *)

val flanagansaxe :
  ?simp:('a -> 'a) ->
  ?less_duplication:bool -> ?k:int -> Gcl.t -> Ast.exp -> Ast.exp
(** [flanagansaxe p q] computes [wp(p,q)] using Flanagan and Saxe's algorithm. *)

(** {5 Directionless Weakest Precondition Algorithms} *)

val dwp_1st :
  ?simp:('a -> 'a) ->
  ?less_duplication:bool ->
  ?k:int -> Gcl.t -> Ast.exp -> Ast.var list * Ast.exp
(** [dwp_1st p q] returns a tuple [(vars, pc)] where [pc] is [wp(p,q)]
    and [vars] is a list of variables that should be quantified using
    foralls. *)

val dwp :
  ?simp:('a -> 'a) ->
  ?less_duplication:bool -> ?k:int -> Gcl.t -> Ast.exp -> Ast.exp
(** [dwp p q] is the same as {!dwp_1st}, except it generates a
    precondition that does not need quantifiers.  However, it can only be
    used to test for satisfiability; it {b cannot} be used to test for
    validity.  Also, the formula is false for [Assume] statements that
    are false, as these are generally not interesting. *)

val dwp_let :
  ?simp:('a -> 'a) ->
  ?less_duplication:bool -> ?k:int -> Gcl.t -> Ast.exp -> Ast.exp
(** [dwp_let] is just like {!dwp}, except that [dwp_let] wraps helper
    variables in [Let] expressions so they do not appear as free
    variables. *)
