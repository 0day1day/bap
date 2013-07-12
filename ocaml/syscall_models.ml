(**

   A module with IL models of system calls.

*)

open Ast

(**

   We are going to index each system call model by the value of eax.
   Because this is going to be quite time-consuming we will use (for
   now) specials for system call models that are unimplemented.

*)

let x86_is_system_call = function
  | Special(("int 80"|"syscall"), _) -> true
  | _ -> false

let syscall_reg = function
  | Type.X86_32 -> Disasm_i386.R32.eax
  | Type.X86_64 -> Disasm_i386.R64.rax
  | _ -> failwith "syscall_reg: unsupported architecture"

(* System call names - fill in as needed *)
let linux_get_name arch syscall =
  match arch with
  | Type.X86_32 -> 
    (match syscall with
    | 1 -> "exit"
    | 3 -> "read"
    | 4 -> "write"
    | 5 -> "open"
    | 6 -> "close"
    | 33 -> "access"
    | 45 -> "brk"
    | 54 -> "ioctl"
    | 91 -> "munmap"
    | 102 -> "socketcall"
    | 122 -> "uname"
    | 125 -> "mprotect"
    | 146 -> "writev"
    | 175 -> "sigprocmask"
    | 192 -> "mmap2"
    | 195 -> "stat64"
    | 197 -> "fstat64"
    | 199 -> "getuid32"
    | 200 -> "getgid32"
    | 201 -> "geteuid32"
    | 202 -> "getegid32"
    | 221 -> "fcntl64"
    | 224 -> "gettid"
    | 240 -> "futex"
    | 243 -> "set_thread_area"
    | 252 -> "exit_group"
    | 270 -> "tgkill"
    | n -> "unknown syscall #" ^ string_of_int n
    )
  | Type.X86_64 -> 
    (match syscall with
    | 0 -> "read"
    | 1 -> "write"
    | 2 -> "open"
    | 3 -> "close"
    | 4 -> "stat"
    | 5 -> "fstat"
    | 9 -> "mmap" 
    | 10 -> "mprotect"
    | 11 -> "munmap"
    | 12 -> "brk"
    | 14 -> "sigprocmask"
    | 16 -> "ioctl"
    | 20 -> "writev"
    | 21 -> "access"
    | 60 -> "exit"
    | 63 -> "uname"
    | 72 -> "fcntl"
    | 102 -> "getuid" 
    | 104 -> "getgid"
    | 107 -> "geteuid"
    | 108 -> "getegid"
    | 186 -> "gettid"
    | 202 -> "futex"
    | 205 -> "set_thread_area"
    | 231 -> "exit_group"
    | 234 -> "tgkill"
    | n -> "unknown syscall #" ^ string_of_int n
    )
  | _ -> failwith "linux_get_name: unsupported architecture"

(* Fill in system call models as needed *)
let linux_get_model arch syscall = 
  match arch with
  | Type.X86_32 -> 
    (match syscall with 
    | 1 ->
        (* exit *)
        (* Exit code is in ebx *)
      Some(Halt(Var Disasm_i386.R32.ebx, [])
           :: [])
    | 252 ->
      (* exit group *)
      Some(Halt(Var Disasm_i386.R32.ebx, [])
           :: [])
    | _ ->
       None
    )
  | Type.X86_64 -> 
    (match syscall with
    | 60 ->
        (* exit *)
        (* Exit code is in rdi *)
      Some(Halt(Var Disasm_i386.R64.rdi, [])
           :: [])
    | 231 ->
      (* exit group *)
      Some(Halt(Var Disasm_i386.R64.rdi, [])
           :: [])
    | _ ->
       None
    )
  | _ -> failwith "linux_get_name: unsupported architecture"
 

let linux_syscall_to_il arch rax =
  match linux_get_model arch rax with
    | Some model ->
      Comment((linux_get_name arch rax) ^ " model", [])
      :: model
    | None ->
        let sys_name = linux_get_name arch rax in
        let mode = match arch with
          | Type.X86_32 -> Disasm_i386.X86
          | Type.X86_64 -> Disasm_i386.X8664
          | _ -> failwith "linux_syscall_to_il: unsupported architecture"
        in
        Special (sys_name, [])
        :: Move(syscall_reg arch, Unknown("System call output", Disasm_i386.type_of_mode mode), [])
        :: []
