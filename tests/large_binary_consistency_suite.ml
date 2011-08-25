open OUnit
open Pcre
open TestCommon


let binary = "../coreutils32_static/ls";;
let binary_args = "";;
let log_file = "large_binary_test.log";;


let pin_trace_setup _ =
  let args = 
	(" -t "^(gentrace_path^gentrace)^" -logall-before -o "^pin_out_suffix^" -- "
	 ^binary^" "^binary_args) in
  (*let args = 
	["-t"; (gentrace_path^gentrace); 
	 "-o"; pin_out_suffix; "--"; binary; binary_args ] in
  let exit_code = Unix.WEXITED(1) in*)
  long_check();
  check_pin_setup();
  check_file (pin_path^pin);
  check_file (gentrace_path^gentrace);
  check_file (stp_path^stp);
  (*assert_command ~exit_code (pin_path^pin) args;*)
  ignore(Sys.command (pin_path^pin^args));
  find_pin_out (Array.to_list (Sys.readdir "./"));;


let pin_trace_test pin_out = 
  let prog = Asmir.bap_from_trace_file ~pin:true pin_out in
  let oc = open_out log_file in  
  let log = fun x -> output_string oc x in
  Traces.consistency_check := true;
  Traces.checkall := true;
  ignore(Traces.concrete ~log prog);
  close_out oc;
(*  Traces.consistency_check := false;
  set_stp_path();
  ignore(Traces.output_exploit exploit_file prog)*)
;;


(* Note: This will leave the files pin.log and pintool.log by intention *)
let pin_trace_cleanup pin_out = 
  ();;(*Sys.remove pin_out;;*)


let suite = "Pin" >:::
  [
	"large_binary_consistency_test" >::
	  bracket pin_trace_setup pin_trace_test pin_trace_cleanup;
  ]
