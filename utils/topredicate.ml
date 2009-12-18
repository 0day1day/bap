
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


let compute_dwp cfg post =
  let (gcl, _, tossa) = Gcl.passified_of_astcfg cfg in
  let p = rename_astexp tossa post in
  (Wp.dwp gcl p, [])


let compute_wp = ref compute_wp_boring
let irout = ref(Some stdout)
let stpout = ref None
let post = ref "true"


let speclist =
  ("-o", Arg.String (fun f -> irout := Some(open_out f)),
   "<file> Print output to <file> rather than stdout.")
  ::("-stp-out", Arg.String (fun f -> stpout := Some(open_out f)),
   "<file> Print output to <file> rather than stdout.")
  ::("-q", Arg.Unit (fun () -> irout := None),
   "Quiet: Supress outputting the WP in the BAP IL.")
  ::("-post", Arg.Set_string post,
     "<exp> Use <exp> as the postcondition (defaults to \"true\")")
  ::("-dwp", Arg.Unit(fun()-> compute_wp := compute_dwp),
     "Use efficient directionless weakest precondition instead of the default")
  ::("-dwp1", Arg.Unit(fun()-> compute_wp := compute_dwp1),
     "Use 1st order efficient directionless weakest precondition")
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
    let p = new Stp.pp_oc oc in
    let () = p#assert_ast_exp_with_foralls foralls wp in
    p#close
