let usage = "Usage: "^Sys.argv.(0)^" <options>\nTry to insert \"halt true\" into a BAP IL file."

open Pcre

let in_il = ref None
and out_il = ref None
and ret_addr = ref None

let speclist =
  [
	("-in",
	 Arg.String(fun s -> in_il := Some(s)),
	 "<file> Input IL file to add \"halt true\" to");
	("-out",
	 Arg.String(fun s -> out_il := Some(s)),
	 "<file> Output IL file (copy of -in <file> with \"halt true\" added)");
	(*("-addr",
	 Arg.String(fun s -> ret_addr := Some(Int64.of_string s)),
	 "<0xADDRESS> ADDRESS where the main function starts.");*)	
  ];;

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"));;
let () = Arg.parse speclist anon usage;;

(**
  1. Find & print out line which occurs before where we want to put halt true 
  2. Output new file with "halt true" in proper location
  3. Return address of Main (begining of execution point)
**)

(* Find start and end addresses of main *)
let p = Asmir.open_program
