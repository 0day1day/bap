(**
   Interface for executing command line driven SMT solvers.

   XXX: This module is designed for Unix systems and is not
   portable.

   @author ejs
*)

(** The result of solving a formula. *)
type result = Valid (** The formula was valid or unsatisfiable. *)
              | Invalid (** The formula was invalid or satisfiable. *)
              | SmtError of string (** The solver failed.  Possible reasons for this include the formula having invalid syntax and the solver running out of memory. *)
              | Timeout (** The solver took too long to solve the formula. *)
val result_to_string : result -> string
  (** Convert a result to a string *)

(** A hack so that we can subtype solver instances. If ocaml <3.11 had
    first order modules, we wouldn't need this. *)
class type smtexec =
object
  method printer : Formulap.fppf
  method solvername : string
  method solve_formula_file : ?timeout:int -> ?remove:bool -> ?printmodel:bool -> string -> result
end

module type SOLVER =
sig
  val solvername : string (** Solver name *)
  val solve_formula_file : ?timeout:int -> ?remove:bool -> ?printmodel:bool -> string -> result 
  (** [solve_formula_file f] solves the formula in [f]. 
      @param timeout Sets the timeout duration in seconds.
      @param remove If set, remove the formula after solving it.
      @param printmodel If set, prints a satisfiable model if one is found. 
  *)
  val check_exp_validity : ?timeout:int -> ?remove:bool -> ?exists:(Ast.var list) -> ?foralls:(Ast.var list) -> Ast.exp -> result 
  (** [check_exp_validity e] tests the validity of [e]. The [timeout] and
      [remove] options are the same as in {!solve_formula_file}.
      @param exists A list of variables to be existentially quantified at the front of the expression.
      @param foralls A list of variables to be quantified with foralls at the front of the expression.
  *)
  (* XXX: check_exp_sat *)
  val create_cfg_formula :
    ?remove:bool -> ?exists:Ast.var list ->  ?foralls:Ast.var list -> Cfg.AST.G.t -> string
  (** [create_cfg_formula cfg] computes the weakest precondition for
      the CFG program [cfg], using postcondition [true]. The weakest
      precondition is then written to a file, and the name of this file is
      returned.

      [remove], [exists], and [foralls] behave the same as above.

      XXX: Select weakest precondition method

      XXX: Give this a better name
  *)

  val si : smtexec
    (** An object to enable subtyping *)
end
(** Interface for a solver. *)

module STP : SOLVER
module STPSMTLIB : SOLVER
module CVC3 : SOLVER
module CVC3SMTLIB : SOLVER
module YICES : SOLVER

(** A Hashtbl that maps solver names to the corresponding {!SOLVER} module. *)
val solvers : (string,smtexec) Hashtbl.t
