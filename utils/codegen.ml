(**
   $Id$

   Code generation tester.
*)

let usage = "Usage: "^Sys.argv.(0)^" [options] <expression to be compiled>\n\
             Translate programs to the IL. "

let exp = ref None
let out = ref None

let speclist =
  ("-o", Arg.String (fun f -> out := Some(open_out f)),
   "<file> Print output to <file> rather than stdout.")
  ::[]

let anon x = match !exp with
  | None -> exp := Some(fst(Parser.exp_from_string x))
  | Some _ -> raise (Arg.Bad("Expected only one anonymous argument"))
let () = Arg.parse speclist anon usage

let () = if !exp = None then Arg.usage speclist ("The expression to be compiled was not specified\n"^usage);;

let exp = match !exp with | Some(x) -> x | None -> failwith "impossible"
let () = Printf.printf "llvm: "; flush stdout; Llvm.dump_value (Llvm_codegen.codegen_exp_helper exp)
let () = Printf.printf "bap: %s\n" (Pp.ast_exp_to_string exp)
