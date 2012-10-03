(* Verification conditions

   XXX: Rename functions
*)

module D=Debug.Make(struct let name = "Vc" and default = `NoDebug end)
open D
open Utils_common
open Type

type options = {
  cf : bool;
  k : int;
  mode : formula_mode;
  full_subst : bool;
}

let default_options = {
  cf = true;
  k = 1;
  mode = Sat;
  full_subst = true;
}

type ast_vc = options -> Ast.program -> Ast.exp -> Ast.exp * Ast.var list
type cfg_vc = options -> Cfg.AST.G.t -> Ast.exp -> Ast.exp * Ast.var list
type ssa_vc = options -> Cfg.SSA.G.t -> Ast.exp -> Ast.exp * Ast.var list

type t = | AstVc of ast_vc
         | CfgVc of cfg_vc
         | SsaVc of ssa_vc

let vc_astprog vc options prog post  = match vc with
  | AstVc vc -> vc options prog post
  | CfgVc vc -> vc options (Cfg_ast.of_prog prog) post
  | SsaVc vc ->
    let cfg = Cfg_ast.of_prog prog in
    let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
    let post = rename_astexp tossa post in
    vc options cfg post

let vc_astcfg vc options prog post  = match vc with
  | AstVc vc -> vc options (Cfg_ast.to_prog prog) post
  | CfgVc vc -> vc options prog post
  | SsaVc vc ->
    let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg prog in
    let post = rename_astexp tossa post in
    vc options cfg post

let vc_ssacfg vc options prog post  = match vc with
  | AstVc vc -> vc options (Cfg_ssa.to_ast prog) post
  | CfgVc vc -> vc options (Cfg_ssa.to_astcfg prog) post
  | SsaVc vc -> vc options prog post

let compute_dwp1 _ cfg post =
  let (gcl, foralls) = Gcl.passified_of_ssa Foralls cfg in
  let (moreforalls, wp) = Wp.dwp_1st gcl post in
  (wp, moreforalls@foralls)
let compute_dwp1_gen = SsaVc compute_dwp1

let compute_wp _ cfg post =
  let gcl = Gcl.of_astcfg cfg in
  (Wp.wp gcl post, [])
let compute_wp_gen = CfgVc compute_wp

let compute_uwp _ cfg post =
  let ugcl = Ugcl.of_ssacfg cfg in
  (Wp.uwp ugcl post, [])
let compute_uwp_gen = SsaVc compute_uwp

let compute_uwp_efficient {mode=mode} cfg post =
  let ugcl = Ugcl.of_ssacfg ~passify:mode cfg in
  (Wp.efficient_uwp ugcl post, [])
let compute_uwp_efficient_gen = SsaVc compute_uwp_efficient

let compute_dwp {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa mode cfg in
  (Wp.dwp ~k mode gcl post, foralls)
let compute_dwp_gen = SsaVc compute_dwp

let compute_dwp_let {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa mode cfg in
  (Wp.dwp_let ~k mode gcl post, foralls)
let compute_dwp_let_gen = SsaVc compute_dwp_let

let compute_eddwp {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa mode cfg in
  (Dwp.eddwp ~k mode gcl post, foralls)
let compute_eddwp_gen = SsaVc compute_eddwp

let compute_eddwp_conc {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa mode ~rm_assigns:false cfg in
  (Dwp.eddwp_conc ~k mode gcl post, foralls)
let compute_eddwp_conc_gen = SsaVc compute_eddwp_conc

let compute_eddwp_lazyconc {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa mode ~rm_assigns:false cfg in
  (Dwp.eddwp_lazyconc ~k mode gcl post, foralls)
let compute_eddwp_lazyconc_gen = SsaVc compute_eddwp_lazyconc

let compute_flanagansaxe {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa mode cfg in
  (Wp.flanagansaxe ~k mode gcl post, foralls)
let compute_flanagansaxe_gen = SsaVc compute_flanagansaxe

let compute_fse_unpass {cf=cf} cfg post =
  let efse = Efse.of_astcfg cfg in
  (Efse.fse_unpass ~cf efse post, [])
let compute_fse_unpass_gen = CfgVc compute_fse_unpass

let compute_fse_pass {cf=cf; mode=mode} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.fse_pass ~cf efse post mode, [])
let compute_fse_pass_gen = CfgVc compute_fse_pass

let compute_efse_pass {cf=cf; mode=mode} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse ~cf efse post mode, [])
let compute_efse_pass_gen = CfgVc compute_efse_pass

let compute_efse_mergepass {cf=cf; mode=mode} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse_merge1 ~cf efse post mode, [])
let compute_efse_mergepass_gen = CfgVc compute_efse_mergepass

let compute_efse_lazypass {cf=cf; mode=mode} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse_lazy ~cf efse post mode, [])
let compute_efse_lazypass_gen = CfgVc compute_efse_lazypass

(* let compute_efse_feaspass cfg post = *)
(*   let cfg, post = optimize_cfg ~usedc:!usedc ~usesccvn:!usesccvn cfg post in *)
(*   let (efse, tossa) = Efse.passified_of_astcfg cfg in *)
(*   let post = rename_astexp tossa post in *)
(*   (Efse.efse_feas ~cf:!dwpcf efse post, []) *)

(* end DWP paper *)

let compute_fse_bfs {full_subst=full_subst} ast post =
  let bfs = if not full_subst then Symbeval_search.bfs_ast_program_fast
    else Symbeval_search.bfs_ast_program
  in
  (bfs ast post, [])
let compute_fse_bfs_gen = AstVc compute_fse_bfs

let compute_fse_bfs_maxdepth i {full_subst=full_subst} ast post =
  let bfs = if not full_subst then Symbeval_search.bfs_maxdepth_ast_program_fast
    else Symbeval_search.bfs_maxdepth_ast_program
  in
  (bfs i ast post, [])
let compute_fse_bfs_maxdepth_gen i = AstVc (compute_fse_bfs_maxdepth i)

let compute_fse_maxrepeat i {full_subst=full_subst} ast post =
  let maxrepeat = if not full_subst then Symbeval_search.maxrepeat_ast_program_fast
    else Symbeval_search.maxrepeat_ast_program
  in
  (maxrepeat i ast post, [])
let compute_fse_maxrepeat_gen i = AstVc (compute_fse_maxrepeat i)

let vclist =
  ("dwp", compute_dwp_gen)
  :: ("eddwp", compute_eddwp_gen)
  :: ("eddwp_conc", compute_eddwp_conc_gen)
  :: ("dwplet", compute_dwp_let_gen)
  :: ("dwp1", compute_dwp1_gen)
  :: ("flanagansaxe", compute_flanagansaxe_gen)
  :: ("wp", compute_wp_gen)
  :: ("uwp", compute_uwp_gen)
  :: ("uwpe", compute_uwp_efficient_gen)
  (* :: ("fse-unpass", compute_fse_unpass_gen) *)
  (* :: ("fse-pass", compute_fse_pass_gen) *)
  (* :: ("efse-pass", compute_efse_pass_gen) *)
  (* :: ("efse-mergepass", compute_efse_mergepass_gen) *)
  (* :: ("efse-lazypass", compute_efse_lazypass_gen) *)
  :: ("fse-bfs", compute_fse_bfs_gen)
  :: []

let pred_vclist =
  ("dwp", compute_dwp_gen)
  :: ("eddwp", compute_eddwp_gen)
  :: ("eddwp_conc", compute_eddwp_conc_gen)
  :: ("dwplet", compute_dwp_let_gen)
  :: ("flanagansaxe", compute_flanagansaxe_gen)
  :: ("wp", compute_wp_gen)
  :: ("uwp", compute_uwp_gen)
  :: ("uwpe", compute_uwp_efficient_gen)
  (* :: ("fse-unpass", compute_fse_unpass_gen) *)
  (* :: ("fse-pass", compute_fse_pass_gen) *)
  (* :: ("efse-pass", compute_efse_pass_gen) *)
  (* :: ("efse-mergepass", compute_efse_mergepass_gen) *)
  (* :: ("efse-lazypass", compute_efse_lazypass_gen) *)
  :: ("fse-bfs", compute_fse_bfs_gen)
  :: []
