
let usage = "Usage: "^Sys.argv.(0)^" <input options> [-o output]\n\
             Translate programs to the IL. "

let compute_wp cfg post =
  let gcl = Gcl.of_astcfg cfg in
  Wp.wp gcl post

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

let wp = compute_wp cfg post

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
    let () = p#assert_ast_exp wp in
    p#close
