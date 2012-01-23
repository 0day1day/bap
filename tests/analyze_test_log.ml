open Pcre
open BatListFull

let usage = "Usage: "^Sys.argv.(0)^" <-l logfile> [-o output]\n\
             Collect statistics for a logfile from the long test.";;

let log = ref stdin;;
let out = ref stdout;;
let verbose = ref false;;

let total_run_time = ref "0";;
let block_count = ref "0";;
let total_traces = ref 0;;

let bap_unknown_hashtbl = Hashtbl.create 100;;
let total_unknown = ref 0;;
let total_wrong = ref 0;;
let probably_right_hashtbl = Hashtbl.create 100;;
let small_hashtbl = Hashtbl.create 100;;

let speclist =
  [("-l", Arg.String (fun f -> log := open_in f),
	"<logfile> Open <logfile> rather than stdin.");
   ("-o", Arg.String (fun f -> out := open_out f),
	"<file> Print output to <file> rather than stdout.");
   ("-v", Arg.Set verbose,
	"Turn on verbose output (display full assembly commands).")];;

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"));;
let () = Arg.parse speclist anon usage;;


let print_out str = output_string !out (str^"\n"); flush !out;;

let one_decimal_percent a b = 
  (ceil ((float_of_int (a * 1000)) /. (float_of_int b))) /. (float_of_int 10)

(* Print the key value pairs in order *)
let print_hashtbl ?(total = None) table =
  let create_list key value k = (key,value)::k in
  let hash_list = Hashtbl.fold create_list table [] in
  let print_item (key,value) = 
    match total with
    | None -> print_out (key^" : "^(string_of_int value))
    | Some(x) -> 
      print_out (key^" : "^(string_of_int value)^" : "
		 ^(string_of_float (one_decimal_percent value x)^"%"))
  in
  (* Print list in descending order *)
  let cmp (_,value1) (_,value2) = compare value2 value1 in
  ignore(List.map print_item (List.sort ~cmp hash_list));;


let insert_item hashtbl item =
  let count = if(Hashtbl.mem hashtbl item) 
	then Hashtbl.find hashtbl item else 0 in
  Hashtbl.replace hashtbl item (count+1);;


(* regexps for matching general information *)
let total_run_time_regexp = regexp "tests in: ([0-9]+\.[0-9]+) seconds\.$";;
let total_blocks_regexp = regexp "^Trace(Eval|s): Running block: ([0-9]+)";;
let trace_count_regexp = regexp "^LongNightly: Processing trace-file";;

(* regexps for matching incorrect and unknown assembley instructions *)
let bap_regexp = regexp "^AsmirTest: BAP unknown disasm_instr \S+: disasm_i386: unimplemented feature: unsupported opcode: (.*)$";;
let vex_regexp = regexp "vex x86->IR: unhandled instruction bytes: (.*)$";;
let trace_eval_regexp = regexp "^WARNING \(Trace(Eval|s)\): Difference between BAP and trace values for \[\*(R_\S*)\* Trace=(\S*) Eval=(\S*)\]";;
let trace_eval_regexp2 = regexp "^WARNING \(Trace(Eval|s)\): This is due to one of the following statments:";;
let stmt_regexp = regexp "^\{addr .*asm (\".*?\") (.*)\}$";;
let asm_regexp = regexp "^\"(.*)\"";;
let first_word_regexp = regexp "^(\S*)";;
let special_regexp = regexp "(int|sysenter)";;

let get_match rex text = 
  let matches = extract ~rex ~full_match:false text in
  Array.get matches 0;;

let wrong_register = ref None;;
let asm_buff = ref [];;

(* Process list sorts asm instructions into def problem, and might be problems 
   ignoring "bad" instructions like syscalls *)
let process_list asms =
  if (List.exists (fun (asm,_) -> pmatch ~rex:special_regexp asm) asms)
  then (
    List.map 
      (fun (asm,r) -> 
	let asm = (get_match first_word_regexp asm)^" "^r in
	insert_item probably_right_hashtbl asm;
	(* insert_item probably_right_hashtbl (asm^" "^r); *)
	if (!verbose) then (
	  print_out "Probably got right, but just in case:";
    	  print_out ("Asm = "^asm);
	  (* print_out ("Context = "^Array.get stmts 1); *)
	)
      )
      asms
  )
  else (
    (* No special indicates these were instructions we definitely got wrong *)
    List.map 
      (fun (asm,r) -> 
	let asm = (get_match first_word_regexp asm)^" "^r in 
      insert_item small_hashtbl asm; 
	(*insert_item small_hashtbl (asm^" "^r);*)
	incr total_wrong;
	if (!verbose) then (
    	  print_out "Definitly got wrong:";
    	  print_out ("Asm = "^asm);
	  (* print_out ("Context = "^Array.get stmts 1); *)
	)
      )
      asms
  );;

(* SWXXX Also collect stats on:
   Run time per binary
   Average size of binarys (number of instructions processed)
*)
let process_line l = (
  match !wrong_register with
  | Some(r) -> (
    (* Ignore the second warnning line *)
    if (pmatch ~rex:trace_eval_regexp2 l) then () else
      try
	let stmts = extract ~rex:stmt_regexp ~full_match:false l in
    	let asm = get_match asm_regexp (Array.get stmts 0) in
	asm_buff := (asm,r)::(!asm_buff)
      with Not_found -> 
	process_list !asm_buff;
	(* Reset counters as we are no longer analyzing the list of bad
	   instructions *)
	wrong_register := None;
	asm_buff := [];
  )
  | None -> (
    try
      let matches = extract ~rex:bap_regexp ~full_match:false l in
      let bap_instr = Array.get matches 0 in
      insert_item bap_unknown_hashtbl bap_instr;
      incr total_unknown;
    (*if (!verbose) then 
      print_out ("Unknown BAP instructions: "^bap_instr);*)
    with Not_found -> (
      try
	let matches = extract ~rex:vex_regexp ~full_match:false l in
	let vex_instr = Array.get matches 0 in
	insert_item small_hashtbl vex_instr;
	if (!verbose) then 
	  print_out ("Unknown VEX instructions: "^vex_instr);
      with Not_found -> (
	try
	  let matches =
	    extract ~rex:trace_eval_regexp ~full_match:false l 
	  in
	  wrong_register := Some(Array.get matches 1);
	  if (!verbose) then (
	    print_out "The following belongs to the below lines:";
	    print_out ("Reg = "^Array.get matches 1);
	    print_out ("Trace = "^Array.get matches 2);
	    print_out ("Eval = "^Array.get matches 3);
	  )
	(* Take care of general stats *)
	with Not_found ->  (
	  try 
	    let matches =
	      extract ~rex:total_run_time_regexp ~full_match: false l 
	    in
	    total_run_time := Array.get matches 0
	  with Not_found -> (
	    try 
	      let matches =
		extract ~rex:total_blocks_regexp ~full_match: false l 
	      in    
	      (* First match is really just an or *)
	      block_count := Array.get matches 1
	    with Not_found -> (
	      if (pmatch ~rex:trace_count_regexp l) then
		(incr total_traces)
	      else () (*print_out ("Unknown line: "^l)*)
	    )
	  )
	)
      ) 
    )
  )
);;


let _ =
  (try
     while true do process_line (input_line !log) done
   with End_of_file -> (close_in !log));
  
  print_out "General Run Stats:";
  print_out ("Total run time : " ^ !total_run_time ^ " seconds");
  print_out ("Number of traces : " ^ string_of_int !total_traces);
  print_out ("Total number of instructions : " ^ !block_count);
  print_out ("Total number of unknown instructions : " 
	     ^ string_of_int !total_unknown);		 
  let unknown_percent = 
    (string_of_float 
       (one_decimal_percent !total_unknown (int_of_string !block_count))) in
  print_out ("Percentage of unknown blocks : "^unknown_percent^"%");
  print_out ("Total number of wrong instructions : " 
	     ^ string_of_int !total_wrong);
  let wrong_percent = 
    (string_of_float 
       (one_decimal_percent !total_wrong (int_of_string !block_count))) in
  print_out ("Percentage of incorrect blocks : "^wrong_percent^"%");
  flush !out;

  print_out "Unknown Instruction Summary:";
  print_hashtbl ~total:(Some(!total_unknown)) bap_unknown_hashtbl;
  flush !out;
  
  print_out "Incorrect Implementation Summary:";
  print_hashtbl ~total:(Some(!total_wrong)) small_hashtbl;
  flush !out;

  print_out "Incorrect but Likely due to special Summary:";
  print_hashtbl probably_right_hashtbl;
  flush !out;

  close_out !out;;
