exception Unimplemented

let disasm_instr arch =
  match arch with
  | Libbfd.Bfd_arch_i386 -> Disasm_i386.disasm_instr
  | _ -> raise Unimplemented
  
