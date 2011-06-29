open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "BAP OUnit Tests" >::: 
  [Il_suite.suite; 
   Var_suite.suite]

let _ =
  run_test_tt_main suite
