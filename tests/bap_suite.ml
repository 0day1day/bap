(*let usage = "Usage: "^Sys.argv.(0)^" <options>\nRun BAP unit tests. "*)

open OUnit
open TestCommon


let short_tests = [
  Il_suite.suite;
  Var_suite.suite;
  Ast_suite.suite;
  Disasm_i386_suite.suite;
  Asmir_suite.suite;
  Traces_suite.suite;
  Pin_suite.suite;
  Predicate_suite.suite;
];;

let long_tests = [
  Large_binary_consistency_suite.suite;
];;


let bap_tests = ("BAP" >::: short_tests@long_tests);;
 

let summarize r =
  match r with 
  | RError(p,s) ->
	Format.printf "Error: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RFailure (p,s) ->
	Format.printf "Failure: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RSkip (p,s) -> 
	Format.printf "Skiped: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RTodo (p,s) -> 
	Format.printf "Todo: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RSuccess p -> ();;

let rec summarize_results res =
  match res with
  | [] -> None
  | r::rs -> summarize r; summarize_results rs;;

let rec print_paths paths =
  match paths with
  | [] -> ()
  | p::ps -> Format.printf "%s\n" (string_of_path p); print_paths ps;;


let _ = 
  let results = run_test_tt_main ~arg_specs:speclist bap_tests in
  Format.printf "%s\n" "";
  summarize_results results
;;
