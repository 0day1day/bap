
let usage = "Usage: "^Sys.argv.(0)^" <input options> [-o output]\n\
             Translate programs to the IL. "

let compute_wp_boring cfg post =
  let gcl = Gcl.of_astcfg cfg in
  (Wp.wp gcl post, [])


(* This may be usefule elsewhere, but I'm not sure where to put it. *)
let rename_astexp f =
  let vis = object
    inherit Ast_visitor.nop
    method visit_rvar v =
      try `ChangeTo(f v)
      with Not_found -> `DoChildren
  end in
  Ast_visitor.exp_accept vis

let compute_dwp1 cfg post =
  let (gcl, foralls, tossa) = Gcl.passified_of_astcfg cfg in
  let p = rename_astexp tossa post in
  let (moreforalls, wp) = Wp.dwp_1st gcl p in
  (wp, moreforalls@foralls)


let compute_dwp ?(k=1) cfg post =
  let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let p = rename_astexp tossa post in
  let vars = Stp.freevars p in
  let cfg = Ssa_simp.simp_cfg ~liveout:vars cfg in
  let (gcl, _) = Gcl.passified_of_ssa cfg in
  (Wp.dwp ~k gcl p, [])

(* FIXME: Why did I think we needed SSA here? *)
let compute_fse cfg post =
  let {Cfg_ssa.cfg=ssa; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let ast = Cfg_ssa.to_ast ssa in
  let p = rename_astexp tossa post in
  (Eval.fse p ast, [])

let compute_wp = ref compute_wp_boring
let fast_fse = ref false
let irout = ref(Some stdout)
let post = ref "true"
let stpout = ref None
let pstpout = ref None
let suffix = ref ""

let compute_fse_bfs cfg post =
  (* FIXME: avoid converting to cfg *)
  let ast = Cfg_ast.to_prog cfg in
  let bfs = if !fast_fse then Symbeval_search.bfs_ast_program_fast
            else Symbeval_search.bfs_ast_program 
  in 
  (bfs ast post, [])

let compute_fse_bfs_maxdepth i cfg post =
  (* FIXME: avoid converting to cfg *)
  let ast = Cfg_ast.to_prog cfg in
  let bfs = if !fast_fse then Symbeval_search.bfs_maxdepth_ast_program_fast
            else Symbeval_search.bfs_maxdepth_ast_program 
  in 
  (bfs i ast post, [])

let compute_fse_maxrepeat i cfg post =
  (* FIXME: avoid converting to cfg *)
  let ast = Cfg_ast.to_prog cfg in
  let maxrepeat = if !fast_fse then Symbeval_search.maxrepeat_ast_program_fast
            else Symbeval_search.maxrepeat_ast_program 
  in 
  (maxrepeat i ast post, [])



let speclist =
  ("-o", Arg.String (fun f -> irout := Some(open_out f)),
   "<file> Print output to <file> rather than stdout.")
  ::("-stp-out", Arg.String (fun f -> stpout := Some(open_out f)),
   "<file> Print output to <file> rather than stdout.")
  ::("-pstp-out", Arg.String (fun f -> pstpout := Some(open_out f)),
   "<file> Print WP expression without assertion to <file>.")
  ::("-q", Arg.Unit (fun () -> irout := None),
   "Quiet: Supress outputting the WP in the BAP IL.")
  ::("-post", Arg.Set_string post,
     "<exp> Use <exp> as the postcondition (defaults to \"true\")")
  ::("-suffix", Arg.String (fun str -> suffix := str),
     "<suffix> Add <suffix> to each variable name.")
  ::("-dwp", Arg.Unit(fun()-> compute_wp := compute_dwp),
     "Use efficient directionless weakest precondition instead of the default")
  ::("-dwpk", Arg.Int(fun i-> compute_wp := compute_dwp ~k:i),
     "Use efficient directionless weakest precondition instead of the default")
  ::("-dwp1", Arg.Unit(fun()-> compute_wp := compute_dwp1),
     "Use 1st order efficient directionless weakest precondition")
  ::("-fse", Arg.Unit(fun()-> compute_wp := compute_fse),
     "Use naive forward symbolic execution (no loops)")
  ::("-fse-bfs", Arg.Unit(fun()-> compute_wp := compute_fse_bfs),
     "Use naive forward symbolic execution with breath first search")
  ::("-fse-bfs-maxdepth", Arg.Int(fun i-> compute_wp := compute_fse_bfs_maxdepth i),
     "<n> FSE with breath first search, limiting search depth to n.")
  ::("-fse-maxrepeat", Arg.Int(fun i-> compute_wp := compute_fse_maxrepeat i),
     "<n> FSE excluding walks that visit a point more than n times.")
  ::("-fast-fse", Arg.Unit(fun () -> fast_fse := true),
     "Perform FSE without full substitution.")
    :: Input.speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage


let prog =
  try Input.get_program()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let post = Parser.exp_from_string !post

let cfg = Cfg_ast.of_prog prog
let cfg = Prune_unreachable.prune_unreachable_ast cfg

let (wp, foralls) = !compute_wp cfg post

;;
match !irout with
| None -> ()
| Some oc ->
    let p = new Pp.pp_oc oc in
    let () = p#ast_exp wp in
    p#close
;;
match !stpout with
| None -> ()
| Some oc ->
    let m2a = new Memory2array.memory2array_visitor () in
    let wp = Ast_visitor.exp_accept m2a wp in
    let foralls = List.map (Ast_visitor.rvar_accept m2a) foralls in
    let p = new Stp.pp_oc ~suffix:!suffix oc in
    let () = p#assert_ast_exp_with_foralls foralls wp true in
    p#close
;;
match !pstpout with
| None -> ()
| Some oc ->
    let m2a = new Memory2array.memory2array_visitor () in
    let wp = Ast_visitor.exp_accept m2a wp in
    let foralls = List.map (Ast_visitor.rvar_accept m2a) foralls in
    let p = new Stp.pp_oc ~suffix:!suffix oc in
    let () = p#assert_ast_exp_with_foralls foralls wp false in
    p#close
;;

