open OUnit
open Pcre
open Test_common


let binary_dir = "static_bins/";;
let args_dir = "static_args/";;
let log_file_suffix = "_large_binary_test.log";;


let file_list dir = 
  let c = ref true in
  let dirs = ref [] in
  let dir_hand = Unix.opendir dir in
  while !c do
	try
	  let file = Unix.readdir dir_hand in
	  if (file <> ".." && file <> "." && file <> "CVS" 
		  && String.sub file 0 1 <> ".")
	  then dirs := file::(!dirs)
	with End_of_file -> (Unix.closedir dir_hand; c := false)
  done;
  !dirs;;
  

let get_args binary =
  let args_file = args_dir^binary^".args" in
  print_endline ("Opening args file "^args_file);
  BatInnerIO.read_all (BatFile.open_in args_file);;


let gen_trace binary =
  let binary_args = get_args binary in
  (* XXX This does not scale; whats a better way to do this? *)
  let args = if (binary = "nohup") then " -follow_execv " else " " in
  let args = 
	(args^"-t "^(gentrace_path^gentrace)^" -logall-before -o "^binary^
		pin_out_suffix^" -- "^binary_dir^binary^" "^binary_args) in
  print_endline ("Executing cmd "^pin_path^pin^args);
  ignore(Sys.command (pin_path^pin^args));
  find_pin_out (Array.to_list (Sys.readdir "./")) binary;;
 

let run_concrete pin_out binary = 
  print_endline ("Processing binary "^binary^" with trace-file "^pin_out);
  let prog = Asmir.bap_from_trace_file ~pin:true pin_out in
  let oc = open_out (binary^log_file_suffix) in
  let log = fun x -> output_string oc x in
  typecheck prog;
  Traces.consistency_check := true;
  Traces.checkall := true;
  ignore(Traces.concrete ~log prog);
  close_out oc;;


let pin_trace_setup _ =
  long_check();
  check_pin_setup();
  check_file (pin_path^pin);
  check_file (gentrace_path^gentrace);
  check_file (stp_path^stp);
  mkdir_and_ignore("/tmp/rmdir");
  mkdir_and_ignore("/tmp/tmp");
  file_list binary_dir;;


let pin_trace_test bins = 
  let pin_outs = 
	if (!long_bins <> "") then
	  let file_list = Array.to_list (Sys.readdir !long_bins) in
	  let find_existing_pin_out binary =
		!long_bins^"/"^(find_pin_out file_list binary) in
	  List.map find_existing_pin_out bins
	else List.map gen_trace bins 
  in
  List.iter2 run_concrete pin_outs bins;
  pin_outs
(*  Traces.consistency_check := false;
  set_stp_path();
  ignore(Traces.output_exploit exploit_file prog)*)
;;


(* Note: This will leave the files pin.log and pintool.log by intention *)
let pin_trace_cleanup pin_outs = 
  let to_delete = ["/tmp/tmp" ; "/tmp/rmdir" ; "/tmp/xcp" ; "/tmp/xginstall" 
				  ; "/tmp/xx" ; "/tmp/xxlink" ; "/tmp/xxln" ; "/tmp/xxmkfifo" 
				  ; "/tmp/xxmknod" ; "xaa" ; "xx00" ; "xx01"] in
  ignore(rm_and_ignore_list to_delete);
;;(*List.map Sys.remove pin_outs;;*)


let suite = "Pin" >:::
  [
	"large_binary_consistency_test" >::
	  bracket pin_trace_setup pin_trace_test pin_trace_cleanup;
  ]
