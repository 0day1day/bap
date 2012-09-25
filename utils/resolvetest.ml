open Ast
open Type
open Utils_common

let usage = "Usage: "^Sys.argv.(0)^" <Binary>\n\
             Test program to resolve indirect jumps"

let speclist = []

let arg = ref 0;;
let binname = ref None;;
let fname = ref None;;

let anon x =
  (match !arg with
  | 1 -> fname := Some x
  | 0 -> binname := Some x
  | _ -> failwith "Expected two arguments");
  incr arg;;

let () = Arg.parse speclist anon usage;;

if !arg < 1 then
  (Arg.usage speclist usage;
   exit 1);;

let asmp = Asmir.open_program (BatOption.get !binname);;

let funcs = Asmir.get_function_ranges asmp;;

let lift_func (n,s,e) =
  let go = match !fname with
    | Some x when n = x -> true
    | Some _ -> false
    | None _ -> true
  in
  if go then (
    Printf.printf "Lifting %s\n" n;
    flush stdout;
    let cfg,_ = Asmir_disasm.vsa_at asmp s in
    let cfg = Hacks.ast_remove_indirect cfg in
    Cfg_pp.AstStmtsDot.output_graph (open_out ("resolve"^n^".dot")) cfg)

let funcs = List.iter lift_func funcs;;

