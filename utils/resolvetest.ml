open Ast
open Type
open Utils_common

let usage = "Usage: "^Sys.argv.(0)^" <IL> <Binary>\n\
             Test program to resolve indirect jumps"

let speclist = []

let arg = ref 0;;
let ilname = ref None;;
let binname = ref None;;

let anon x =
  (match !arg with
  | 0 -> ilname := Some x
  | 1 -> binname := Some x
  | _ -> failwith "Expected two arguments");
  incr arg;;

let () = Arg.parse speclist anon usage;;

(* let m2a_state = Memory2array.create_state () *)

if !arg <> 2 then
  (Arg.usage speclist usage;
   exit 1);;

let prog,scope = Parser.program_from_file (BatOption.get !ilname);;

let asmp = Asmir.open_program (BatOption.get !binname);;

let cfg = Cfg_ast.of_prog prog

let () = ignore(Resolve_indirect.resolve_indjumps asmp cfg);;
