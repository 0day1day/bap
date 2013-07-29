type arch =
  | X86_32
  | X86_64

let arch_to_string = function
  | X86_32 -> "X86_32"
  | X86_64 -> "X86_64"

let type_of_arch = function
  | X86_32 -> Type.Reg 32
  | X86_64 -> Type.Reg 64

let mode_of_arch = function
  | X86_32 -> Disasm_i386.X86
  | X86_64 -> Disasm_i386.X8664
