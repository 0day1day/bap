(** Supported BAP architectures *)

type arch =
  | X86_32
  | X86_64

val arch_to_string : arch -> string
