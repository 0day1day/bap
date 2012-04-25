(**
   $Id$

   Code generation tester.
*)

open Ast
open Big_int_convenience
open Llvm_executionengine

let usage = "Usage: "^Sys.argv.(0)^" <input options> [options]\n\
             Translate programs to the IL. "

let out = ref None
let exec = ref false

let speclist =
  ("-o", Arg.String (fun f -> out := Some(open_out f)),
   "<file> Print output to <file> rather than stdout.")
  :: ("-exec", Arg.Set exec,
      "Execute the generated code. (DANGEROUS)")
  :: Input.speclist

let anon x =
  raise (Arg.Bad("Expected only one anonymous argument"))
let () = Arg.parse speclist anon usage

let prog,scope =
  try Input.get_program()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let cfg = Cfg_ast.of_prog prog;;
let cfg = Prune_unreachable.prune_unreachable_ast cfg;;

(* let prog = Memory2array.coerce_prog prog *)

(* let () = *)
(*   List.iter (fun s -> *)
(*     List.iter (fun s -> *)
(*       Printf.printf "%s\n" (Pp.ast_stmt_to_string s) *)
(*     ) (Flatten_mem.flatten_stores s) *)
(*   ) prog *)

let () =
  let codegen = new Llvm_codegen.codegen Llvm_codegen.FuncMulti in
  let f = codegen#convert_cfg cfg in
  (* let f = codegen#convert_straightline_f prog in *)
  (* Llvm.dump_value f; *)
  let ctx = [(Disasm_i386.esp, Int(biconst 1234, reg_32))] in
  codegen#dump;
  if !exec then
    Printf.printf "result: %d\n" (GenericValue.as_int (codegen#run_fun ~ctx f));


(*   Printf.printf "llvm: "; flush stdout; Llvm.dump_value (codegen#convert_exp_helper exp) *)
(* let () = Printf.printf "bap: %s\n" (Pp.ast_exp_to_string exp); *)
(*   Gc.full_major() *)
