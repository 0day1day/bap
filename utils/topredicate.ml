open Ast
open Grammar_scope
open Type
open Utils_common

let usage = "Usage: "^Sys.argv.(0)^" <input options> [-o output]\n\
             Translate programs to the IL. "

let fast_fse = ref false
let irout = ref(Some stdout)
let post = ref "true"
let stpout = ref None
let stpoutname = ref ""
let pstpout = ref None
let suffix = ref ""
let assert_vars = ref false
let usedc = ref true
let usesccvn = ref true
let solve = ref false

(* Select which solver to use *)
let solver = ref (Smtexec.STP.si);;

let compute_dwp1 cfg post =
  let (gcl, foralls, tossa) = Gcl.passified_of_astcfg cfg in
  let p = rename_astexp tossa post in
  let (moreforalls, wp) = Wp.dwp_1st gcl p in
  (wp, moreforalls@foralls)


let to_ssapassgcl cfg post =
  let cfg = Hacks.remove_cycles cfg in
  let cfg = Coalesce.coalesce_ast cfg in
  let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let p = rename_astexp tossa post in
  let cfg =
    let vars = Formulap.freevars p in
    Ssa_simp.simp_cfg ~liveout:vars ~usedc:!usedc ~usesccvn:!usesccvn cfg
  in
  let (gcl, _) = Gcl.passified_of_ssa cfg in
  (gcl, p)

let to_ugcl passify cfg post =
  let cfg = Hacks.remove_cycles cfg in
  let cfg = Coalesce.coalesce_ast cfg in
  let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let p = rename_astexp tossa post in
  let cfg =
    let vars = Formulap.freevars p in
    Ssa_simp.simp_cfg ~liveout:vars ~usedc:!usedc ~usesccvn:!usesccvn cfg
  in
  let gcl = Ugcl.of_ssacfg ~passify:passify cfg in
  (gcl, p)

let compute_wp_boring cfg post =
  let (gcl, post) = to_ssagcl ~usedc:!usedc ~usesccvn:!usesccvn cfg post in
  (Wp.wp gcl post, [])

let compute_uwp_boring cfg post =
  let (ugcl, p) = to_ugcl false cfg post in
  (Wp.uwp ugcl p, [])

let compute_uwp_passify cfg post =
  let (ugcl, p) = to_ugcl true cfg post in
  (Wp.efficient_uwp ugcl p, [])

let compute_dwp ?(k=1) cfg post =
  let (gcl,p) = to_ssapassgcl cfg post in
  (Wp.dwp ~k gcl p, [])

let compute_flanagansaxe ?(k=1) cfg post =
  let (gcl,p) = to_ssapassgcl cfg post in
  (Wp.flanagansaxe ~k gcl p, [])

let extract_vars e =
  let rec h v = function
    | BinOp(AND, BinOp(EQ,Var v1, e1), BinOp(EQ,Var v2, e2)) ->
	((v2,e2)::(v1,e1)::v, None)
    | BinOp(AND, BinOp(EQ,Var v1, e1), rest)
    | BinOp(AND, rest, BinOp(EQ,Var v1, e1)) ->
	h ((v1,e1)::v) rest
    | BinOp(AND, e1, e2) ->
	(match h v e1 with
	 | (v, None) ->
	     h v e2
	 | (v, Some e1) ->
	     match h v e2 with
	     | (v, None) -> (v, Some e1)
	     | (v, Some e2) -> (v, Some(exp_and e1 e2))
	)
    | e -> (v, Some e)
  in
  match h [] e with
  | (v, Some e) -> (v,e)
  | (v, None) -> (v, exp_true)


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

let set_solver s =
  solver := try Hashtbl.find Smtexec.solvers s
  with Not_found ->
    failwith "Unknown solver"

let solvers = Hashtbl.fold (fun k _ s -> k ^ " " ^ s) Smtexec.solvers ""

let compute_wp = ref compute_wp_boring

let speclist =
  ("-o", Arg.String (fun f -> irout := Some(open_out f)),
   "<file> Print output to <file> rather than stdout.")
  ::("-stp-out", Arg.String (fun f -> stpoutname := f; stpout := Some(open_out f)),
   "<file> Print output to <file> rather than stdout.")
  ::("-pstp-out", Arg.String (fun f -> pstpout := Some(open_out f)),
   "<file> Print WP expression without assertion to <file>.")
  ::("-q", Arg.Unit (fun () -> irout := None),
   "Quiet: Supress outputting the WP in the BAP IL.")
  ::("-post", Arg.Set_string post,
     "<exp> Use <exp> as the postcondition (defaults to \"true\")")
  ::("-suffix", Arg.String (fun str -> suffix := str),
     "<suffix> Add <suffix> to each variable name.")
  ::("-wp", Arg.Unit(fun()-> compute_wp := compute_wp_boring),
     "Use Dijkstra's WP, except with let instead of substitution.")
  ::("-uwp", Arg.Unit(fun()-> compute_wp := compute_uwp_boring),
     "Use WP for Unstructured Programs")
  ::("-uwpe", Arg.Unit(fun()-> compute_wp := compute_uwp_passify),
     "Use efficient WP for Unstructured Programs")
  ::("-dwp", Arg.Unit(fun()-> compute_wp := compute_dwp),
     "Use efficient directionless weakest precondition instead of the default")
  ::("-dwpk", Arg.Int(fun i-> compute_wp := compute_dwp ~k:i),
     "Use efficient directionless weakest precondition instead of the default")
  ::("-dwp1", Arg.Unit(fun()-> compute_wp := compute_dwp1),
     "Use 1st order efficient directionless weakest precondition")
  ::("-flanagansaxe", Arg.Unit(fun()-> compute_wp := compute_flanagansaxe),
     "Use Flanagan & Saxe's algorithm instead of the default WP.")
  ::("-solver", Arg.String set_solver,
     ("Use the specified solver. Choices: " ^ solvers))
  ::("-extract-vars", Arg.Set assert_vars,
     "Put vars in separate asserts")
  ::("-fse-bfs", Arg.Unit(fun()-> compute_wp := compute_fse_bfs),
     "Use naive forward symbolic execution with breath first search")
  ::("-fse-bfs-maxdepth", Arg.Int(fun i-> compute_wp := compute_fse_bfs_maxdepth i),
     "<n> FSE with breath first search, limiting search depth to n.")
  ::("-fse-maxrepeat", Arg.Int(fun i-> compute_wp := compute_fse_maxrepeat i),
     "<n> FSE excluding walks that visit a point more than n times.")
  ::("-fast-fse", Arg.Set fast_fse,
     "Perform FSE without full substitution.")
  ::("-noopt", Arg.Unit (fun () -> usedc := false; usesccvn := false),
     "Do not perform any optimizations on the SSA CFG.")
  ::("-opt", Arg.Unit (fun () -> usedc := true; usesccvn := true),
     "Perform all optimizations on the SSA CFG.")
  ::("-optdc", Arg.Unit (fun () -> usedc := true; usesccvn := false),
     "Perform deadcode elimination on the SSA CFG.")
  ::("-optsccvn", Arg.Unit (fun () -> usedc := false; usesccvn := true),
     "Perform sccvn on the SSA CFG.")
  ::("-solve", Arg.Unit (fun () -> solve := true),
     "Solve the generated formula.")
  :: Input.speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let m2a_state = Memory2array.create_state ()

let prog,scope =
  try Input.get_program()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let post,scope = Parser.exp_from_string ~scope !post

let prog = Memory2array.coerce_prog_state ~scope m2a_state prog
let post = Memory2array.coerce_exp_state ~scope m2a_state post

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
    let foralls = List.map (Memory2array.coerce_rvar_state ~scope m2a_state) foralls in 
    let pp = (((!solver)#printer) :> Formulap.fppf) in
    let p = pp ~suffix:!suffix oc in
    if !assert_vars then (
      let (vars,wp') = extract_vars wp in
      List.iter (fun (v,e) -> p#assert_ast_exp (BinOp(EQ, Var v, e))) vars;
      p#assert_ast_exp_with_foralls foralls wp'
    )
    else (
      p#assert_ast_exp_with_foralls foralls wp;
    );
    p#counterexample;
    p#close;
    if !solve then
      let r = (!solver)#solve_formula_file ~printmodel:true !stpoutname in
      Printf.fprintf stderr "Solve result: %s\n" (Smtexec.result_to_string r)

;;
match !pstpout with
| None -> ()
| Some oc ->
    let foralls = List.map (Memory2array.coerce_rvar_state m2a_state) foralls in 
    let pp = (((!solver)#printer) :> Formulap.fppf) in
    let p = pp ~suffix:!suffix oc in
	p#forall foralls;
	p#ast_exp wp;
	p#close
;;

