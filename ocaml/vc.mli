(** Interface to verification generation procedures.  Verification
    condition procedures take as input a program [p] and post-condition
    [q], and return a logical formula that is [true] when executions in
    [p] end in a state satisfying the post-condition [q].

    @author Ed Schwartz
*)

(** {2 VC-specific Types} *)

(** Various options that can modify the behavior of VC generation
    algorithms. *)
type options = {
  cf : bool; (** Perform constant folding (concrete evaluation) during VC generation. Only used by EFSE algorithms. *)
  k : int; (** Expressions larger than [k] will be given their own temporary variable assignment in the formula. Only used by DWP algorithms. *)
  mode : Type.formula_mode; (** Indicates whether the formula should be valid for validity or satisfiability. This is currently only needed for VC algorithms that use passification, since passification introduces new free variables into the formula. *)
  full_subst : bool; (** [true] indicates that full substitution should be performed, even for [Let] expressions, in which case exponential blowup may occur. [false] enables partial substitution only. *)
}

(** Sensible default VC options *)
val default_options : options

(** Type of VC algorithm that naturally operates on a {!Ast.program} program. *)
type ast_vc = options -> Ast.program -> Ast.exp -> Ast.exp * Ast.var list

(** Type of VC algorithm that naturally operates on a {!Cfg.AST.G.t} program. *)
type cfg_vc = options -> Cfg.AST.G.t -> Ast.exp -> Ast.exp * Ast.var list

(** Type of VC algorithm that naturally operates on a {!Cfg.SSA.G.t} program. *)
type ssa_vc = options -> Cfg.SSA.G.t -> Ast.exp -> Ast.exp * Ast.var list

(** Any type of VC algorithm *)
type t = AstVc of ast_vc | CfgVc of cfg_vc | SsaVc of ssa_vc

(** [vc_astprog vc opts prog post] runs [vc] to compute the VC of [prog]
    with post-condition [post], starting from a {!Ast.program} program. *)
val vc_astprog : t -> options -> Ast.program -> Ast.exp -> Ast.exp * Ast.var list

(** [vc_astcfg] is like [vc_astprog], but for AST CFG programs. *)
val vc_astcfg : t -> options -> Cfg.AST.G.t -> Ast.exp -> Ast.exp * Ast.var list

(** [vc_ssacfg] is like [vc_astprog], but for SSA CFG programs. *)
val vc_ssacfg : t -> options -> Cfg.SSA.G.t -> Ast.exp -> Ast.exp * Ast.var list

(** {2 DWP algorithms} *)

(** DWP implementation that does not emit quantifiers. *)
val compute_dwp : ssa_vc
val compute_dwp_gen : t

(** DWP implementation that uses forall quantifiers. *)
val compute_dwp1 : ssa_vc
(** General form of [compute_dwp1]. *)
val compute_dwp1_gen : t

(** Flanagan and Saxe's algorithm, which is like DWP but can only run backwards. *)
val compute_flanagansaxe : ssa_vc
val compute_flanagansaxe_gen : t

(** {2 "Standard" Weakest Precondition Algorithms} *)

(** Weakest precondition based on Dijkstra's GCL. Produces
    exponentially sized formulas. *)
val compute_wp : cfg_vc
val compute_wp_gen : t

(** Unstructured Weakest Precondition that operates on a CFG-like
    representation. Produces exponentially sized formulas. *)
val compute_uwp : ssa_vc
val compute_uwp_gen : t

(** Unstructured Weakest Precondition algorithm that uses
    passification. Output sub-exponentially sized formulas. *)
val compute_uwp_efficient : ssa_vc
val compute_uwp_efficient_gen : t

(** {2 Experimental Efficient VC Generation Algorithms from DWP paper} *)

val compute_fse_unpass : cfg_vc
val compute_fse_unpass_gen : t

val compute_fse_pass : cfg_vc
val compute_fse_pass_gen : t

val compute_efse_pass : cfg_vc
val compute_efse_pass_gen : t

val compute_efse_mergepass : cfg_vc
val compute_efse_mergepass_gen : t

val compute_efse_lazypass : cfg_vc
val compute_efse_lazypass_gen : t

(** {2 Symbolic Execution VC algorithms} *)

(** Use BFS to visit all states. Produces exponentially sized
    formulas. *)
val compute_fse_bfs : ast_vc
val compute_fse_bfs_gen : t

(** Use BFS to visit all states up to a certain depth. Produces
    exponentially sized formulas. *)
val compute_fse_bfs_maxdepth : int -> ast_vc
val compute_fse_bfs_maxdepth_gen : int -> t

(** Use BFS to visit all states on paths that do not visit a node more
    than [k] times. Produces exponentially sized formulas. *)
val compute_fse_maxrepeat : int -> ast_vc
val compute_fse_maxrepeat_gen : int -> t

(** {2 All Supported VCs} *)

(** A list of supported VCs.  VCs with parameters (like maxdepth) are
    not included. *)
val vclist : (string * t) list

(** A list of supported VCs that only require predicate logic (no
    quantifiers). *)
val pred_vclist : (string * t) list
