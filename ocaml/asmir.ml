(** High level interface to libasmir.
    
    The functions in this file should be used instead of calling Libasmir functions
    directly. These functions should be easier to use, and, unlike the Libasmir
    ones, will handle garbage collection.

    @author Ivan Jager
*)

open Libasmir
open Type
open Ast
open Util

type asmprogram = Libasmir.asm_program_t

type arch = Libasmir.bfd_architecture

let arch_i386 = Libasmir.Bfd_arch_i386
let arch_arm  = Libasmir.Bfd_arch_arm
(*more to come later when we support them*)

module D = Debug.Make(struct let name = "ASMIR" and default=`Debug end)
open D

(** Translate a unop *)
let tr_unop = function
  | Libasmir.NEG -> NEG
  | Libasmir.NOT -> NOT

(** Translate a type *)
let tr_regtype = function
  | Libasmir.REG_1   -> REG_1 
  | Libasmir.REG_8   -> REG_8 
  | Libasmir.REG_16  -> REG_16
  | Libasmir.REG_32  -> REG_32
  | Libasmir.REG_64  -> REG_64


(* maps a string variable to the var we are using for it *)
type varctx = (string,Var.t) Hashtbl.t

(** [gamma_create mem decls] creates a new varctx for use during translation. 
    [mem] is the var that should be used for memory references, and [decls]
    should be a list of variables already in scope.
*)
let gamma_create mem decls : varctx =
  let h = Hashtbl.create 57 in
  let () = List.iter (fun (Var.V(_,nm,_) as var) -> Hashtbl.add h nm var) decls in
  let () = Hashtbl.add h "$mem" mem in
  h

let gamma_lookup (g:varctx) s =
  try Hashtbl.find g s
  with Not_found ->
    failwith("Disassembled code had undeclared variable '"^s^"'. Something is broken.")

let gamma_extend = Hashtbl.add


let gamma_unextend = Hashtbl.remove

(* This should really be elsewhere... *)
let little_endian = Int(0L, REG_1)

(* Translate a string label into a name or address label as approriate *)
let tr_label s =
  Name s (* FIXME: treating them all as names for now *)


(** Translate an expression *)
let rec tr_exp g e =
  match Libasmir.exp_type e with
    | BINOP ->
	tr_binop g (Libasmir.binop_type e) (Libasmir.binop_lhs e) (Libasmir.binop_rhs e)
    | UNOP ->
        UnOp(tr_unop(Libasmir.unop_type e),
	     tr_exp g (Libasmir.unop_subexp e) )
    | CONSTANT ->
        Int(Libasmir.constant_val e, tr_regtype (constant_regtype e))
    | MEM ->
	let mem = gamma_lookup g "$mem" in
	let wtyp = tr_regtype (mem_regtype e) in 
	Load(Var mem, tr_exp g (mem_addr e), little_endian, wtyp)
    | TEMP ->
	let nm = temp_name e  in
	let Var.V(_,_,t) as var = gamma_lookup g nm in
	(*let t' = tr_regtype(temp_regtype e) in
	if t <> t'
	then failwith("Disassembly produced incorrect type "^type_to_string t'^" for"^var_to_string var)
	else*) Var var
    | CAST ->
        let sube = tr_exp g (cast_subexp e) in
        let newt = tr_regtype(cast_width e) in
	(match cast_casttype e with
	 | Libasmir.CAST_UNSIGNED -> Cast(CAST_UNSIGNED, newt, sube)
	 | Libasmir.CAST_SIGNED   -> Cast(CAST_SIGNED, newt, sube)
	 | Libasmir.CAST_HIGH	   -> Cast(CAST_HIGH, newt, sube)
	 | Libasmir.CAST_LOW	   -> Cast(CAST_LOW, newt, sube)
	 | Libasmir.CAST_FLOAT
	 | Libasmir.CAST_INTEGER
	 | Libasmir.CAST_RFLOAT
	 | Libasmir.CAST_RINTEGER ->
	     (pwarn "Warning: Ignoring deprecated cast type\n"; sube)
	)
    | NAME ->
	(match tr_label (name_string e) with
	 | Name n -> Lab n
	 | Addr i -> Int(i, REG_64)
	)
    | UNKNOWN ->
        Unknown(unknown_str e, tr_regtype(unknown_regtype e))
    | LET ->
	failwith "Let expressions from C++ no longer supported"
    | EXTENSION ->
	failwith "Extension stmt types are unsupported."
    | _ ->
	failwith "Unexpected stmt type"


(** Translate a binop *)
and tr_binop g b lhs rhs =
  let (lhs,rhs) = (tr_exp g lhs, tr_exp g rhs) in
  match b with
    | Libasmir.PLUS     -> BinOp(PLUS    , lhs, rhs)
    | Libasmir.MINUS	-> BinOp(MINUS   , lhs, rhs)
    | Libasmir.TIMES	-> BinOp(TIMES   , lhs, rhs)
    | Libasmir.DIVIDE	-> BinOp(DIVIDE  , lhs, rhs)
    | Libasmir.SDIVIDE	-> BinOp(SDIVIDE  , lhs, rhs)
    | Libasmir.MOD	-> BinOp(MOD     , lhs, rhs)
    | Libasmir.SMOD	-> BinOp(SMOD     , lhs, rhs)
    | Libasmir.LSHIFT	-> BinOp(LSHIFT  , lhs, rhs)
    | Libasmir.RSHIFT	-> BinOp(RSHIFT  , lhs, rhs)
    | Libasmir.ARSHIFT	-> BinOp(ARSHIFT , lhs, rhs)
    | Libasmir.LROTATE
    | Libasmir.RROTATE	-> failwith "rotate is deprecated"
    | Libasmir.LOGICAND -> BinOp(AND  , lhs, rhs) (* operands should be bool *)
    | Libasmir.LOGICOR	-> BinOp(OR   , lhs, rhs) (* operands should be bool *)
    | Libasmir.BITAND	-> BinOp(AND  , lhs, rhs)
    | Libasmir.BITOR	-> BinOp(OR   , lhs, rhs)
    | Libasmir.XOR	-> BinOp(XOR     , lhs, rhs)
    | Libasmir.EQ	-> BinOp(EQ      , lhs, rhs)
    | Libasmir.NEQ	-> BinOp(NEQ     , lhs, rhs)
        (* FIXME: Assuming all comparisons are unsigned.
           This should be valid for IR generated via VEX. 
        *)
    | Libasmir.LT	-> BinOp(LT, lhs, rhs)
    | Libasmir.LE       -> BinOp(LE, lhs, rhs)
        (* We don't have GT or GTE, so implement using LT and LTE *)
    | Libasmir.GT	-> BinOp(LE, rhs, lhs) (* (x > y) <-> (y <= x) *)
    | Libasmir.GE	-> BinOp(LT, rhs, lhs) (* (x >= y) <-> (y < x) *)


(** Translate a vardecl, and adds the variable to the context
    
    @return vardecl and a function to restore the context
*)
let tr_vardecl (g:varctx) s =
  assert(Libasmir.stmt_type s = VARDECL);
  let nm = Libasmir.vardecl_name s in 
  let var = Var.newvar nm (tr_regtype(Libasmir.vardecl_type s)) in
    gamma_extend g nm var;
    (var, fun () -> gamma_unextend g nm)
    
(** Translate a list of vardecls, adding them to the context.
    @return vardecls and a function to restore the context *)
let tr_vardecls g ss =
  let decls,unextends = List.split(List.map (tr_vardecl g) ss) in
    (decls, fun x -> List.iter (fun f -> f x) unextends)

(** Translate a statement *)
let rec tr_stmt g s =
  match Libasmir.stmt_type s with
      JMP ->
	Jmp(tr_exp g (Libasmir.jmp_target s), [])
    | CJMP ->
	CJmp(tr_exp g (Libasmir.cjmp_cond s),
	    tr_exp g (Libasmir.cjmp_ttarget s),
	    tr_exp g (Libasmir.cjmp_ftarget s),
	    [] )
    | SPECIAL ->
	Special(Libasmir.special_string s, [])
    | MOVE ->
	let e = tr_exp g (move_rhs s) in
	(match tr_exp g (move_lhs s) with
	 | Var v ->
	     Move(v, e, [])
	 | Load(Var var as v, idx, endi, w) ->
	     Move(var, Store(v, idx, e, endi, w), [])
	 | _ -> 
	     failwith "Inproper lvalue in move"
	)
    | COMMENT ->
	Comment(Libasmir.comment_string s, [])
    | LABEL ->
	Label(tr_label (Libasmir.label_string s), [])
    | ASSERT ->
	Assert(tr_exp g (Libasmir.assert_cond s), [])
    | VARDECL
    | EXPSTMT
    | CALL
    | RETURN
    | FUNCTION ->
	failwith "Unsupported statement type"

(** Translate a whole vine_block_t (as returned by
    Libasmir.asmir_vine_blocks_get) into a list of statements *)
let tr_vine_block_t g asmp b = 
  let size = Libasmir.asmir_vine_block_size b - 1 in
  let addr = Libasmir.asmir_vine_block_address b in
  let asm = Libasmir.asmir_string_of_insn asmp addr in
  let (decs,stmts) =
    foldn (fun (ds,ss) n -> let s = asmir_vine_block_get b n in
	     match Libasmir.stmt_type s with
		 VARDECL -> (s::ds,ss)
	       | _ -> (ds,s::ss) )
      ([],[]) size
  in
  let decls, unextend = tr_vardecls g decs in
  let stmts = List.map (tr_stmt g) stmts in
  let stmts = Label(Addr addr, [Asm asm])::stmts in 
  unextend();
  stmts

(** Translate a vine_blocks_t (as returned by
    Libasmir.asmir_asmprogram_to_vine) into a list of statements *)
let tr_vine_blocks_t g asmp bs = 
  let size = Libasmir.asmir_vine_blocks_size bs -1 in
    foldn (fun i n -> tr_vine_block_t g asmp (asmir_vine_blocks_get bs n)@i) [] size




let x86_regs : var list =
  List.map (fun (n,t) -> Var.newvar n t)
    [
  (* 32 bit regs *)
  ("R_EBP", REG_32);
  ("R_ESP", REG_32);
  ("R_ESI", REG_32);
  ("R_EDI", REG_32);
  ("R_EIP", REG_32);
  ("R_EAX", REG_32);
  ("R_EBX", REG_32);
  ("R_ECX", REG_32);
  ("R_EDX", REG_32);
  ("EFLAGS", REG_32);

  (* condition flag bits *)
  ("R_CF", REG_1);
  ("R_PF", REG_1);
  ("R_AF", REG_1);
  ("R_ZF", REG_1);
  ("R_SF", REG_1);
  ("R_OF", REG_1);

  (* VEX left-overs from calc'ing condition flags *)
  ("R_CC_OP", REG_32);
  ("R_CC_DEP1", REG_32);
  ("R_CC_DEP2", REG_32);
  ("R_CC_NDEP", REG_32);

  (* more status flags *)
  ("R_DFLAG", REG_32);
  ("R_IDFLAG", REG_32);
  ("R_ACFLAG", REG_32);
  ("R_EMWARN", REG_32);
  ("R_LDT", REG_32); 
  ("R_GDT", REG_32); 

  (* segment regs *)
  ("R_CS", REG_16); 
  ("R_DS", REG_16); 
  ("R_ES", REG_16); 
  ("R_FS", REG_16); 
  ("R_GS", REG_16); 
  ("R_SS", REG_16); 

  (* floating point *)
  ("R_FTOP", REG_32);
  ("R_FPROUND", REG_32);
  ("R_FC3210", REG_32);
]


(* exectrace needs fixing if this is REG_64 *)
let x86_mem = Var.newvar "mem" (TMem(REG_32))


let arm_regs =
  List.map (fun n -> Var.newvar n REG_32)
    [ "R0";     
      "R1";     
      "R2";     
      "R3";     
      "R4";     
      "R5";     
      "R6";     
      "R7";     
      "R8";     
      "R9";     
      "R10";    
      "R11";    
      "R12";    
      "R13";    
      "R14";    
      "R15";    
      "CC";
      "CC_OP";	 
      "CC_DEP1";
      "CC_DEP2";
    ]

let decls_for_arch = function
  | Bfd_arch_i386 -> x86_mem::x86_regs
  | Bfd_arch_arm  -> x86_mem::arm_regs
  | _ -> failwith "decls_for_arch: unsupported arch"

let gamma_for_arch = function
  | Bfd_arch_i386 -> gamma_create x86_mem x86_regs
  | Bfd_arch_arm  -> gamma_create x86_mem arm_regs
  | _ -> failwith "gamma_for_arch: unsupported arch"


let get_asmprogram_arch = Libasmir.asmir_get_asmp_arch


(** Open a binary file for translation *)
let open_program filename =
  let prog = Libasmir.asmir_open_file filename in
    (* tell the GC how to free resources associated with prog *)
  let () = Gc.finalise Libasmir.asmir_close prog in
  prog

(** Translate an entire Libasmir.asm_program_t into a Vine program *)
let asmprogram_to_vine ?(init_mem=false) asmp = 
  let vine_blocks = Libasmir.asmir_asmprogram_to_vine asmp in
  let arch = get_asmprogram_arch asmp in
  let g = gamma_for_arch arch in
  let ir = tr_vine_blocks_t g asmp vine_blocks in
  let () = destroy_vine_blocks vine_blocks in
  ir



(** Translate only one address of a  Libasmir.asm_program_t to Vine *)
let asm_addr_to_vine g prog addr =
  let block= Libasmir.asmir_addr_to_vine prog addr in
  let ir = tr_vine_block_t g prog block in
  let () = destroy_vine_block block in
  ir
