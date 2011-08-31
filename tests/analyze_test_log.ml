open Pcre

let usage = "Usage: "^Sys.argv.(0)^" <-l logfile> [-o output]\n\
             Collect statistics for a logfile from the long test.";;

let log = ref stdin;;
let out = ref stdout;;
let speclist =
  [("-l", Arg.String (fun f -> log := open_in f),
	"<logfile> Open <logfile> rather than stdin.");
   ("-o", Arg.String (fun f -> out := open_out f),
	"<file> Print output to <file> rather than stdout.")];;

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"));;
let () = Arg.parse speclist anon usage;;

let regxp2 = regexp "^vex x86->IR: unhandled instruction bytes: (.*)$";;
let regxp3 = regexp "^WARNING \(TraceEval\): Difference between BAP and trace values in .*asm (\".*?\") .*context (.*)\]: (R_\S*) Trace=(\S*) Eval=(\S*)$";;

let process_asm text = ();;
  


let process_line l = (
  try
	let matches = extract ~rex:regxp2 ~full_match:false l in
	print_endline "First exp result:";
	print_endline (Array.get matches 0);
  with Not_found -> (
	try
	  let matches = extract ~rex:regxp3 ~full_match:false l in
	  print_endline "Second exp result:";
	  print_endline ("Asm = "^Array.get matches 0);
	  print_endline ("Context = "^Array.get matches 1);
	  print_endline ("Reg = "^Array.get matches 2);
	  print_endline ("Trace = "^Array.get matches 3);
	  print_endline ("Eval = "^Array.get matches 4);
	with Not_found -> (print_endline ("Unknown line: "^l))
  )
);;
  

let _ =
  try
	while true do process_line (input_line !log) done
  with End_of_file -> close_in !log;;
  
