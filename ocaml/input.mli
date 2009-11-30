(** Use this to read in a program.

    TODO: Add convenience functions to get SSA directly, and maybe more input
    options.
*)

open Arg

(** A speclist suitable to pass to Arg.parse.
    Add this to the speclist for your program. *)
val speclist : (key * spec * doc) list

(** Get the program as specified by the commandline. *)
val get_program : unit -> Ast.program

val init_ro : bool ref
