open OUnit

let concrete_eval_setup _ =
  Asmir.open_program ~loud:false "C/test";;

let concrete_eval_tear_down _ = ();;

let concrete_eval_test ast = 
  Asmir.asmprogram_to_bap ast;;

let suite = "Traces" >:::
  [
	"concrete_eval_test" >:: 
	  (bracket concrete_eval_setup concrete_eval_test concrete_eval_tear_down);
  ]
