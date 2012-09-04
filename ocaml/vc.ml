(* Verification conditions

   XXX: Rename functions
*)

open Utils_common

type options = {
  cf : bool;
  k : int;
  sat : bool;
  full_subst : bool;
}

let default_options = {
  cf = true;
  k = 1;
  sat = true;
  full_subst = true;
}

type ast_vc = options -> Ast.program -> Ast.exp -> Ast.exp * Ast.var list
type cfg_vc = options -> Cfg.AST.G.t -> Ast.exp -> Ast.exp * Ast.var list
type ssa_vc = options -> Cfg.SSA.G.t -> Ast.exp -> Ast.exp * Ast.var list

type t = | AstVc of ast_vc
         | CfgVc of cfg_vc
         | SsaVc of ssa_vc

let sat_or_valid sat = if sat then Efse.Assign.Sat else Efse.Assign.Validity

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
  let (gcl, foralls) = Gcl.passified_of_ssa cfg in
  let (moreforalls, wp) = Wp.dwp_1st gcl post in
  (wp, moreforalls@foralls)
let compute_dwp1_gen = SsaVc compute_dwp1

let compute_wp_boring _ cfg post =
  let gcl = Gcl.of_astcfg cfg in
  (Wp.wp gcl post, [])
let compute_wp_boring_gen = CfgVc compute_wp_boring

let compute_uwp_boring _ cfg post =
  let ugcl = Ugcl.of_ssacfg ~passify:false cfg in
  (Wp.uwp ugcl post, [])
let compute_uwp_boring_gen = SsaVc compute_uwp_boring

let compute_uwp_passify _ cfg post =
  let ugcl = Ugcl.of_ssacfg ~passify:true cfg in
  (Wp.efficient_uwp ugcl post, [])
let compute_uwp_passify_gen = SsaVc compute_uwp_passify

let compute_dwp {k=k} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa cfg in
  (Wp.dwp ~k gcl post, foralls)
let compute_dwp_gen = SsaVc compute_dwp

let compute_flanagansaxe {k=k; sat=sat} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa cfg in
  (Wp.flanagansaxe ~k gcl sat post, foralls)
let compute_flanagansaxe_gen = SsaVc compute_flanagansaxe

let compute_fse_unpass {cf=cf} cfg post =
  let efse = Efse.of_astcfg cfg in
  (Efse.fse_unpass ~cf efse post, [])
let compute_fse_unpass_gen = CfgVc compute_fse_unpass

let compute_fse_pass {cf=cf; sat=sat} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.fse_pass ~cf efse post (sat_or_valid sat), [])
let compute_fse_pass_gen = CfgVc compute_fse_pass

let compute_efse_pass {cf=cf; sat=sat} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse ~cf efse post (sat_or_valid sat), [])
let compute_efse_pass_gen = CfgVc compute_efse_pass

let compute_efse_mergepass {cf=cf; sat=sat} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse_merge1 ~cf efse post (sat_or_valid sat), [])
let compute_efse_mergepass_gen = CfgVc compute_efse_mergepass

let compute_efse_lazypass {cf=cf; sat=sat} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse_lazy ~cf efse post (sat_or_valid sat), [])
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
