(* Verification conditions *)

open Utils_common

type vc_options = {
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

let sat_or_valid sat = if sat then Efse.Assign.Sat else Efse.Assign.Validity

let compute_dwp1 cfg post =
  let (gcl, foralls, tossa) = Gcl.passified_of_astcfg cfg in
  let p = rename_astexp tossa post in
  let (moreforalls, wp) = Wp.dwp_1st gcl p in
  (wp, moreforalls@foralls)

let compute_wp_boring _ cfg post =
  let (gcl, post) = to_ssagcl cfg post in
  (Wp.wp gcl post, [])

let compute_uwp_boring _ cfg post =
  let (ugcl, p) = to_ugcl false cfg post in
  (Wp.uwp ugcl p, [])

let compute_uwp_passify _ cfg post =
  let (ugcl, p) = to_ugcl true cfg post in
  (Wp.efficient_uwp ugcl p, [])

let compute_dwp {k=k} cfg post =
  let (gcl,p) = to_ssapassgcl cfg post in
  (Wp.dwp ~k gcl p, [])

let compute_flanagansaxe {k=k; sat=sat} cfg post =
  let (gcl,p) = to_ssapassgcl cfg post in
  (Wp.flanagansaxe ~k gcl sat p, [])

let compute_fse_unpass {cf=cf} cfg post =
  let efse = Efse.of_astcfg cfg in
  (Efse.fse_unpass ~cf efse post, [])

let compute_fse_pass {cf=cf; sat=sat} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.fse_pass ~cf efse post (sat_or_valid sat), [])

let compute_efse_pass {cf=cf; sat=sat} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse ~cf efse post (sat_or_valid sat), [])

let compute_efse_mergepass {cf=cf; sat=sat} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse_merge1 ~cf efse post (sat_or_valid sat), [])

let compute_efse_lazypass {cf=cf; sat=sat} cfg post =
  let (efse, tossa) = Efse.passified_of_astcfg cfg in
  let post = rename_astexp tossa post in
  (Efse.efse_lazy ~cf efse post (sat_or_valid sat), [])

(* let compute_efse_feaspass cfg post = *)
(*   let cfg, post = optimize_cfg ~usedc:!usedc ~usesccvn:!usesccvn cfg post in *)
(*   let (efse, tossa) = Efse.passified_of_astcfg cfg in *)
(*   let post = rename_astexp tossa post in *)
(*   (Efse.efse_feas ~cf:!dwpcf efse post, []) *)

(* end DWP paper *)

let compute_fse_bfs {full_subst=full_subst} cfg post =
  (* FIXME: avoid converting to cfg *)
  let ast = Cfg_ast.to_prog cfg in
  let bfs = if not full_subst then Symbeval_search.bfs_ast_program_fast
    else Symbeval_search.bfs_ast_program
  in
  (bfs ast post, [])

let compute_fse_bfs_maxdepth i {full_subst=full_subst} cfg post =
  (* FIXME: avoid converting to cfg *)
  let ast = Cfg_ast.to_prog cfg in
  let bfs = if not full_subst then Symbeval_search.bfs_maxdepth_ast_program_fast
    else Symbeval_search.bfs_maxdepth_ast_program
  in
  (bfs i ast post, [])

let compute_fse_maxrepeat i {full_subst=full_subst} cfg post =
  (* FIXME: avoid converting to cfg *)
  let ast = Cfg_ast.to_prog cfg in
  let maxrepeat = if not full_subst then Symbeval_search.maxrepeat_ast_program_fast
    else Symbeval_search.maxrepeat_ast_program
  in
  (maxrepeat i ast post, [])
