open BatPervasives

type arch =
  | X86_32
  | X86_64

let arch_to_string = function
  | X86_32 -> "X86_32"
  | X86_64 -> "X86_64"

let arch_of_string = function
  | "x86" | "x86_32" | "X86_32" -> X86_32
  | "x86-64" | "X86-64" -> X86_64
  | _ -> failwith "arch_of_string: Unknown arch"

let type_of_arch = function
  | X86_32 -> Type.Reg 32
  | X86_64 -> Type.Reg 64

let bits_of_arch a = match (type_of_arch a) with
  | Type.Reg n -> n
  | _ -> failwith "bits_of_arch: impossible"

let bytes_of_arch a =
  let bits = bits_of_arch a in
  assert (bits mod 8 == 0);
  bits / 8

let mode_of_arch = function
  | X86_32 -> Disasm_i386.X86
  | X86_64 -> Disasm_i386.X8664

let mem_of_arch = function
  | X86_32 -> Disasm_i386.R32.mem
  | X86_64 -> Disasm_i386.R64.mem

let sp_of_arch = function
  | X86_32 -> Disasm_i386.R32.esp
  | X86_64 -> Disasm_i386.R64.rsp
