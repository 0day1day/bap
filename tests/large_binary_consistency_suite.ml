open OUnit
open Pcre
open TestCommon

let binary = "../x32_bins/date";;
let binary_args = "";;

let pin_trace_setup _ =
  let args = 
	(" -t "^(gentrace_path^gentrace)^" -logall-before -o "^pin_out_suffix^" -- "
	 ^binary^" "^binary_args) in
  (*let args = 
	["-t"; (gentrace_path^gentrace); 
	 "-o"; pin_out_suffix; "--"; binary; binary_args ] in
  let exit_code = Unix.WEXITED(1) in*)
  check_pin_setup();
  (*assert_command ~exit_code (pin_path^pin) args;*)
  Sys.command (pin_path^pin^args);
  find_pin_out (Array.to_list (Sys.readdir "./"));;


let pin_trace_test pin_out = 
  let prog = Asmir.bap_from_trace_file ~pin:true pin_out in
  Traces.consistency_check := true;
  Traces.checkall := true;
  ignore(Traces.concrete prog);
(*  Traces.consistency_check := false;
  set_stp_path();
  ignore(Traces.output_exploit exploit_file prog)*)
;;


(* Note: This will leave the files pin.log and pintool.log by intention *)
let pin_trace_cleanup pin_out = 
  Sys.remove pin_out;;


let suite = "Pin" >:::
  [
	"pin_binary_test" >:: 
	  bracket (chdir_startup pin_path) (check_file pin) chdir_cleanup;
	"pin_obj_test" >:: 
	  bracket (chdir_startup gentrace_path) (check_file gentrace) chdir_cleanup;
	"stp_binary_test" >::
	  bracket (chdir_startup stp_path) (check_file stp) chdir_cleanup;
	"large_binary_consistency_test" >::
	  bracket pin_trace_setup pin_trace_test pin_trace_cleanup;
  ]
