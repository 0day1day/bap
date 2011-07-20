open OUnit

let concrete_eval_setup _ =
  let out = open_out "C/test.il" in
  let pp = new Pp.pp_oc out in
  let ast = Asmir.open_program ~loud:false "C/test" in
  let prog = Asmir.asmprogram_to_bap ast in
  pp#ast_program prog;
  pp#close;
  assert_command ~verbose:true "./inject_halt.pl" [];
  prog;;

let concrete_eval_tear_down _ = ();;

let concrete_eval_test ast = 
  let prog = Parser.program_from_file "C/test.il.halted" in
  prog;;

let suite = "Traces" >:::
  [
	"concrete_eval_test" >::
	  (bracket concrete_eval_setup concrete_eval_test concrete_eval_tear_down);
  ]
