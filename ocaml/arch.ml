type arch =
  | X86_32
  | X86_64

let arch_to_string = function
  | X86_32 -> "X86_32"
  | X86_64 -> "X86_64"
