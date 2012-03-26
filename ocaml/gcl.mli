(** Dijkstra's Guarded Command Language

    Type declarations for the Guarded Command Language, and functions
    to traslate BAP programs to GCL.

    BAP programs are converted to GCL form to compute the weakest
    precondition, since weakest preconditions are typically defined
    for GCL programs.

    @author: Ivan Jager
*)

open Ast

(** {5 Type of GCL statements} *)

(** A GCL statement.
    [Skip] does nothing.
    [Assign(v,e)] assigns an expression [e] to an lvalue [v].
    [Seq(a,b)] evaluates [a] and then moves on to [b].
    [Choice(a,b)] non-deterministically chooses to execute [a] or [b].
    [Assert e] terminates in failure if [e] is untrue, and otherwise does nothing.
    [Assume e] does not start the program if [e] is untrue, and otherwise does nothing.
*)
type t =
  | Assume of exp
  | Assign of Var.t * exp
  | Assert of exp
  | Choice of t * t
  | Seq of t * t
  | Skip

(* (\** Intermediate form of gcl *\) *)
(* type gclhelp = *)
(*   | CAssign of Cfg.AST.G.V.t *)
(*   | CChoice of exp * gclhelp * gclhelp (\* bb with cjmp, true  and false branches *\) *)
(*   | Cunchoice of gclhelp * gclhelp (\* unfinished choice *\) *)
(*   | CSeq of gclhelp list *)

(* val gclhelp_of_astcfg : ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> gclhelp *)

(** {5 Functions to convert BAP programs to GCL} *)

val of_astcfg : ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> t
(** [of_astcfg cfg] converts the AST CFG [cfg] to GCL form.

    The conversion is non-trivial, since GCL is a structured language,
    but AST CFGs are not. The current implementation proceeds by
    choosing a node [n] in reverse topological order from [cfg].  The
    interesting case is when [n] has two successors [x] and [y].  The
    current implmentation gets the GCL for [x] and [y] and finds the
    longest suffix shared between them and the unique parts of [x] and
    [y] not shared in the suffix. The two GCLs are then merged as
    [Seq(Choice(Seq(Assume(cond), Unique_x), Seq(Assume(not cond),
    Unique_y)), Common_suffix)].  This is not guaranteed to return the
    smallest GCL, but is simple and works well in practice.

    @raise Not_found if [cfg] contains cycles.
*)
val of_ast : Ast.program -> t
(** [of_ast] is the same as {!of_astcfg}, except that it converts an AST
    program instead of an AST CFG. *)

(* val remove_skips : t -> t *)

val passified_of_ssa :
  ?entry:Cfg.SSA.G.V.t -> ?exit:Cfg.SSA.G.V.t -> Cfg.SSA.G.t -> t * var list
(** [passified_of_ssa cfg] converts a SSA CFG [cfg] to a passified GCL
    program.  Passified GCL programs do not contain [Assign(v,e)]
    statements.  Instead, all assignments [Assign(v,e)] are replaced with
    [Assume(v == e)] statements.

    Passification is used by the efficient weakest precondition
    algorithms in BAP, inlcuding DWP ({!Wp.dwp}) and Flanagan and
    Saxe's algorithm ({!Wp.flanagansaxe}).

    @raise Not_found if [cfg] contains cycles.
*)
val passified_of_astcfg :
  ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> t * var list * (Var.t->Var.t)
(** [passified_of_astcfg] is the same as {!passified_of_ssa}, except
    that it takes an ASG CFG as input. *)
