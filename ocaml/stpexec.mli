type result = Valid | Invalid | StpError | Timeout
val result_to_string : result -> string

module type SOLVER_INFO =
sig
  val timeout : int (** Default timeout in seconds *)
  val solvername : string (** Solver name *)
  val cmdstr : string -> string (** Given a filename, produce a command string to invoke solver *)
  val parse_result : string -> string -> Unix.process_status -> result (** Given output, decide the result *)
end

module type SOLVER =
sig
  val solve_formula_file : ?timeout:int -> string -> result (** Solve a formula in a file *)
  val solve_formula_exp : ?timeout:int -> ?exists:(Ast.var list) -> ?foralls:(Ast.var list) -> Ast.exp -> result (** Solve a formula in an exp *)
end

module Make : functor (Module : SOLVER_INFO) -> SOLVER

module STP : SOLVER 
module CVC3 : SOLVER

(** Dump a formula to a file *)
val create_formula :
  ?exists:Ast.var list ->  ?foralls:Ast.var list -> ?remove:bool
  -> Cfg.AST.G.t -> string

(* The following are deprecated, use modules *)

(* val runstp : ?timeout:int -> string -> result *)

(* possibly expose write_formula too if that is useful *)

(* val query_formula : *)
(*   ?timeout:int -> ?exists:Ast.var list ->  ?foralls:Ast.var list *)
(*   -> Ast.exp -> result *)
