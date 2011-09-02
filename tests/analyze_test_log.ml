open Pcre

let usage = "Usage: "^Sys.argv.(0)^" <-l logfile> [-o output]\n\
             Collect statistics for a logfile from the long test.";;

let log = ref stdin;;
let out = ref stdout;;
let verbose = ref false;;

let bap_unknown_tbl = Hashtbl.create 100;;
let expanded_hashtbl = Hashtbl.create 100;;
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

(* Print the key value pairs in order *)
let print_hashtbl table =
  let create_list key value k = (key,value)::k in
  let hash_list = Hashtbl.fold create_list table [] in
  let print_item (key,value) = 
	print_endline (key^" : "^(string_of_int value))
  in
  let cmp (_,value1) (_,value2) = compare value1 value2 in
  List.map print_item (List.sort cmp hash_list);;

let insert_item hashtbl item =
  let count = if(Hashtbl.mem hashtbl item) 
	then Hashtbl.find hashtbl item else 0 in
  Hashtbl.replace hashtbl item (count+1);;

let bap_regexp = regexp "^AsmirTest: BAP unknown disasm_instr \S+: disasm_i386: unimplemented feature: unsupported opcode: (.*)$";;
let vex_regexp = regexp "^vex x86->IR: unhandled instruction bytes: (.*)$";;
let trace_eval_regexp = regexp "^WARNING \(TraceEval\): Difference between BAP and trace values in .*asm (\".*?\") .*context (.*)\]: (R_\S*) Trace=(\S*) Eval=(\S*)$";;

let asm_regexp = regexp "^\"(.*)\"";;
let first_word_regexp = regexp "^(\S*)";;


let get_match rex text = 
  let matches = extract ~rex ~full_match:false text in
  Array.get matches 0;;


let process_line l = (
  try
	let matches = extract ~rex:bap_regexp ~full_match:false l in
	let bap_instr = Array.get matches 0 in
	insert_item bap_unknown_tbl bap_instr;
	if (!verbose) then print_endline ("Unknown BAP instructions: "^bap_instr);
  with Not_found -> (
	try
	  let matches = extract ~rex:vex_regexp ~full_match:false l in
	  let vex_instr = Array.get matches 0 in
	  insert_item small_hashtbl vex_instr;
	  if (!verbose) then print_endline ("Unknown VEX instructions: "^vex_instr);
	with Not_found -> (
	  try
		let matches = extract ~rex:trace_eval_regexp ~full_match:false l in
		let asm = get_match asm_regexp (Array.get matches 0) in 
		insert_item small_hashtbl (get_match first_word_regexp asm);
		if (!verbose) then (
		  insert_item expanded_hashtbl asm;	  
		  print_endline "Second exp result:";
		  print_endline ("Asm = "^asm);
		  print_endline ("Context = "^Array.get matches 1);
		  print_endline ("Reg = "^Array.get matches 2);
		  print_endline ("Trace = "^Array.get matches 3);
		  print_endline ("Eval = "^Array.get matches 4);
		)
	  with Not_found ->  () (*print_endline ("Unknown line: "^l)*)
	)
  )
);;
  

let _ =
  (try
	 while true do process_line (input_line !log) done
   with End_of_file -> (close_in !log));
  if (!verbose) then (
	print_endline("Expanded Summary:");
	ignore(print_hashtbl expanded_hashtbl));
  print_endline("BAP unknown Summary:");
  print_hashtbl bap_unknown_tbl;
  print_endline("Small Summary:");
  print_hashtbl small_hashtbl;;
	
