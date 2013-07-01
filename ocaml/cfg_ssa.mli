(** 
    Static Single Assignment translation

    @todo Export trans_cfg, after finding a good name for it.

    @author Ivan Jager
*)

val v2s : Cfg.SSA.G.V.t -> string

val of_astcfg : ?tac:bool -> Cfg.AST.G.t -> Cfg.SSA.G.t

val of_ast : ?tac:bool -> Ast.program -> Cfg.SSA.G.t
(** Translates a AST program into an SSA CFG. *)

val to_astcfg : ?remove_temps:bool -> ?dsa:bool -> Cfg.SSA.G.t -> Cfg.AST.G.t
(** Convert a SSA CFG to an AST CFG. *)

val to_ast : ?remove_temps:bool -> Cfg.SSA.G.t -> Ast.program
(** Convert a SSA CFG to an AST program. *)


type translation_results = {
  cfg : Cfg.SSA.G.t;
  to_astvar: Var.t -> Var.t; (* Maps SSA vars back to the variable they came from *)
  to_ssavar: Var.t -> Var.t; (* Maps AST vars to SSA at end of exit node. *)
}  

val trans_cfg : ?tac:bool -> Cfg.AST.G.t -> translation_results
(** Translates a CFG into SSA form.

    Returns the new SSA CFG and two
    maps. One from SSA variables to the variables they originally came
    from, and the other from the original variables to what they map
    to at the end of the exit node. Both functions act like the
    identity function for variables that don't map to anything. (Eg,
    for temporary variables introduced by SSA, or variables that
    weren't assigned.)  *)
