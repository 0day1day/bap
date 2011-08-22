open OUnit
open Pcre
open TestCommon

let bof = "C/bof1";;
let taint_file = "tainted_file";;
let exploit_file = "exploit";;


let create_input_file _ =
  let out = open_out taint_file in
  output_string out "helloooooooooooooooooo\n";
  close_out out;;
  

let rec find_pin_out files =
  match files with
  | [] -> assert_failure ("Could not find a file with suffix "^pin_out_suffix)
  | f::fs -> if (pmatch ~pat:pin_out_suffix f) then f else find_pin_out fs;;


let pin_trace_setup _ =
  let args = 
	["-t"; (gentrace_path^gentrace); "-taint-files"; taint_file; 
	 "-o"; pin_out_suffix; "--"; bof; taint_file ] in
  let exit_code = Unix.WEXITED(1) in
  check_pin_setup();
  check_file (pin_path^pin);
  check_file (gentrace_path^gentrace);
  check_stp_path(stp_path^stp);
  create_input_file();
  assert_command ~exit_code (pin_path^pin) args;
  find_pin_out (Array.to_list (Sys.readdir "./"));;


let pin_trace_test pin_out = 
(*  let _ =  in *)
  let prog = Asmir.bap_from_trace_file ~pin:true pin_out in
  Traces.consistency_check := true;
  ignore(Traces.concrete prog);
  Traces.consistency_check := false;
  ignore(Traces.output_exploit exploit_file prog);;


(* Note: This will leave the files pin.log and pintool.log by intention *)
let pin_trace_cleanup pin_out = 
  Sys.remove pin_out; Sys.remove exploit_file; Sys.remove taint_file;;


let suite = "Pin" >:::
  [
	"pin_taint_test" >:: (
	  bracket pin_trace_setup pin_trace_test pin_trace_cleanup;)
  ]
