(*let usage = "Usage: "^Sys.argv.(0)^" <options>\nRun BAP unit tests. "*)

open OUnit

(* Collect the tests of different modules into one test suite *)
let tests =
  ("BAP" >::: 
	  [
		Il_suite.suite;
		Var_suite.suite;
		Ast_suite.suite;
		Disasm_i386_suite.suite;
		Asmir_suite.suite;
		Traces_suite.suite;
		Pin_suite.suite;
		Predicate_suite.suite;
		Large_binary_consistency_suite.suite;
	  ]);;

let verbose = ref false;;

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

(*let speclist =
  [
	("-list-tests",
	 Arg.Unit(fun () -> print_paths (test_case_paths tests);exit 0),
	 "Print the list of tests");
	("-verbose", Arg.Set verbose, "Turn on verbose output");
  ];;

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"));;
let () = Arg.parse speclist anon usage;;*)

let _ = 
  (* let results = 
	run_test_tt_main ~arg_specs:speclist ~verbose:!verbose tests in*)
  let results = run_test_tt_main tests in
  Format.printf "%s\n" "";
  summarize_results results
;;
