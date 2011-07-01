open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "BAP OUnit Tests" >::: 
  [
	Il_suite.suite; 
	Var_suite.suite;
	Ast_suite.suite;
	Disasm_i386_suite.suite;
	Asmir_suite.suite;
  ]

let _ =
  run_test_tt_main suite
