open OUnit
open Pcre
open Test_common

let bof = "C/bof1";;
let taint_file = "tainted_file";;
let exploit_file = "exploit";;
let tag = "pin_suite";;


let create_input_file _ =
  let out = open_out taint_file in
  output_string out "helloooooooooooooooooo\n";
  close_out out;;


let pin_trace_setup _ =
  let args =
	["-t"; (gentrace_path^gentrace); "-taint-files"; taint_file;
	 "-o"; tag^pin_out_suffix; "--"; bof; taint_file ] in
  let exit_code = Unix.WEXITED(0) in
  check_pin_setup();
  (* check_file (pin_path^pin); *)
  (* check_file (gentrace_path^gentrace); *)
  check_stp_path();
  create_input_file();
  assert_command ~exit_code (!pin_path^pin) args;
  find_pin_out (Array.to_list (Sys.readdir "./")) tag;;


let pin_trace_test pin_out =
  let prog = Asmir.serialized_bap_from_trace_file pin_out in
  typecheck prog;
  Traces.consistency_check := true;
  ignore(Traces.concrete prog);
  Traces.consistency_check := false;
  ignore(Traces.output_exploit exploit_file prog);;


(* Note: This will leave the files pin.log and pintool.log by intention *)
let pin_trace_cleanup pin_out =
  rm_and_ignore_list [pin_out ; exploit_file ; taint_file];;
(*  Sys.remove pin_out; Sys.remove exploit_file; Sys.remove taint_file;; *)


let suite = "Pin" >:::
  [
	"pin_taint_test" >:: (
	  bracket pin_trace_setup pin_trace_test pin_trace_cleanup;)
  ]
