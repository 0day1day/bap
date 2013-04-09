(** General disassembly stuff *)

exception Unimplemented

let disasm_instr arch mach =
  match arch with
  | Libbfd.Bfd_arch_i386 when mach=Libbfd.mACH_i386_i386 -> Disasm_i386.disasm_instr Disasm_i386.X86
  | Libbfd.Bfd_arch_i386 when mach=Libbfd.mACH_i386_x86_64 -> Disasm_i386.disasm_instr Disasm_i386.X8664
  | _ -> raise Unimplemented

let is_temp = Var_temp.is_temp

let is_decode_error = function
  | Ast.Special("VEX decode error", []) -> true
  | _ -> false
