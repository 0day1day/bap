(** General disassembly stuff *)

exception Unimplemented

let disasm_instr mode = Disasm_i386.disasm_instr mode

let is_temp = Var_temp.is_temp

let is_decode_error = function
  | Ast.Special(s, _) when BatString.starts_with s "Unknown instruction" -> true
  | _ -> false
