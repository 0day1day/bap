open Ast
open Type
open Utils_common

let usage = "Usage: "^Sys.argv.(0)^" <Binary>\n\
             Test program to resolve indirect jumps"

let speclist = []

let arg = ref 0;;
let binname = ref None;;
let entry = ref None;;

let anon x =
  (match !arg with
  | 1 -> entry := Some (Int64.of_string x)
  | 0 -> binname := Some x
  | _ -> failwith "Expected two arguments");
  incr arg;;

let () = Arg.parse speclist anon usage;;

if !arg < 1 then
  (Arg.usage speclist usage;
   exit 1);;

let asmp = Asmir.open_program (BatOption.get !binname);;

let start = match !entry with
  | Some x -> x
  | None -> Asmir.get_start_addr asmp;;

let cfg,_ = Asmir_disasm.recursive_descent_at asmp start;;

let () = ignore(Resolve_indirect.resolve_indjumps asmp cfg);;
