(** Supported BAP architectures *)

type arch =
  | X86_32
  | X86_64

val arch_to_string : arch -> string

val type_of_arch : arch -> Type.typ

val mode_of_arch : arch -> Disasm_i386.mode
