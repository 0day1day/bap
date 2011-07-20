let usage = "Usage: "^Sys.argv.(0)^" <options>\nTry to insert \"halt true\" into a BAP IL file."

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
	("-addr",
	 Arg.String(fun s -> ret_addr := Some(Int64.of_string s)),
	 "<file> Input IL file to add \"halt true\" to");	
  ];;

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"));;
let () = Arg.parse speclist anon usage;;
