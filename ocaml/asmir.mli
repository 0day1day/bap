(** High level interface to libasmir.
    
    The functions in this file should be used instead of calling Libasmir functions
    directly. These functions should be easier to use, and, unlike the Libasmir
    ones, will handle garbage collection.

    @author Ivan Jager
*)

(* This interface is a work-in-progress. I just created it now to avoid exposing
   some variables I added. --aij
*)


type asmprogram = Libasmir.asm_program_t

type arch
val arch_i386 : arch
val arch_arm : arch



type varctx
(*
val gamma_create : Ast.decl -> Ast.decl list -> varctx
val gamma_lookup : varctx -> string -> Var.t
val gamma_extend : varctx -> string -> Ast.decl -> unit
val gamma_unextend : varctx -> string -> unit
*)

(*
val tr_exp : varctx -> Libasmir.exp -> Ast.exp
val tr_binop :
  varctx ->
  Libasmir.binop_type_t -> Libasmir.exp -> Libasmir.exp -> Ast.exp
val tr_vardecl : varctx -> Libasmir.stmt -> Var.t * (unit -> unit)
val tr_vardecls :
  varctx -> Libasmir.stmt list -> Var.t list * (unit -> unit)
val tr_stmt : varctx -> Libasmir.stmt -> Ast.stmt
val tr_bap_block_t :
  varctx -> asmprogram -> Libasmir.bap_block_t -> Ast.stmt list
val tr_bap_blocks_t :
  varctx ->
  asmprogram -> Libasmir.bap_blocks_t -> Ast.stmt list
*)


val decls_for_arch : arch -> Ast.var list
val gamma_for_arch : arch -> varctx

val get_asmprogram_arch : asmprogram -> arch


val open_program : string -> asmprogram
val asmprogram_to_bap : ?init_ro:bool -> asmprogram -> Ast.program
val asm_addr_to_bap :
  varctx -> asmprogram -> Libasmir.address_t -> Ast.program

val asmprogram_to_bap_range : ?init_ro:bool ->
  asmprogram -> Libasmir.address_t -> Libasmir.address_t  -> Ast.program
