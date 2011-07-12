let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Run BAP unit tests. "

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
	  ]);;


let analyze r =
  match r with 
  (*| RSuccess _ | RSkip _ -> ()*)
  | RError(p,s) ->
	Format.printf "Error: %s\n" ((string_of_path p) ^ " : " ^ s)
  | RFailure (p,s) ->
	Format.printf "Failure: %s\n" ((string_of_path p) ^ " : " ^ s)
  | RSkip (p,s) -> 
	Format.printf "Skip: %s\n" ((string_of_path p) ^ " : " ^ s)
  | RTodo (p,s) -> 
	Format.printf "Todo: %s\n" ((string_of_path p) ^ " : " ^ s)
  | _ -> ();;

let rec analyze_results res =
  match res with
  | [] -> None
  | r::rs -> analyze r; analyze_results rs;;

(* match (analyze r) with
	| Some(_) -> (analyze_results rs);; *)

let rec print_paths paths =
  match paths with
  | [] -> ()
  | p::ps -> Format.printf "%s\n" (string_of_path p); print_paths ps;;

let speclist =
  [
	("-list-tests",
	 Arg.Unit(fun () -> print_paths (test_case_paths tests);exit 0),
	 "Print the list of tests")
  ];;

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"));;
let () = Arg.parse speclist anon usage;;

let _ = 
  let results =
	run_test_tt tests
  in
  Format.printf "%s\n" "";
  analyze_results results
;;
