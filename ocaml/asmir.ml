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
  | Libasmir.REG_1   -> reg_1 
  | Libasmir.REG_8   -> reg_8 
  | Libasmir.REG_16  -> reg_16
  | Libasmir.REG_32  -> reg_32
  | Libasmir.REG_64  -> reg_64


(* maps a string variable to the var we are using for it *)
type varctx = (string,Var.t) Hashtbl.t

(** [gamma_create mem decls] creates a new varctx for use during translation. 
    [mem] is the var that should be used for memory references, and [decls]
    should be a list of variables already in scope.
*)
let gamma_create mem decls : varctx =
  let h = Hashtbl.create 57 in
  List.iter (fun (Var.V(_,nm,_) as var) -> Hashtbl.add h nm var) decls;
  Hashtbl.add h "$mem" mem;
  h

let gamma_lookup (g:varctx) s =
  try Hashtbl.find g s
  with Not_found ->
    failwith("Disassembled code had undeclared variable '"^s^"'. Something is broken.")

let gamma_extend = Hashtbl.add


let gamma_unextend = Hashtbl.remove

(* This should really be elsewhere... *)
let little_endian = Int(0L, reg_1)

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
	 | Addr i -> Int(i, reg_64)
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

(** Translate a whole bap_block_t (as returned by
    Libasmir.asmir_bap_blocks_get) into a list of statements *)
let tr_bap_block_t g asmp b = 
  let size = Libasmir.asmir_bap_block_size b - 1 in
  let addr = Libasmir.asmir_bap_block_address b in
  let asm = Libasmir.asmir_string_of_insn asmp addr in
  let (decs,stmts) =
    foldn (fun (ds,ss) n -> let s = asmir_bap_block_get b n in
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

(** Translate a bap_blocks_t (as returned by
    Libasmir.asmir_asmprogram_to_bap) into a list of statements *)
let tr_bap_blocks_t g asmp bs = 
  let size = Libasmir.asmir_bap_blocks_size bs -1 in
    foldn (fun i n -> tr_bap_block_t g asmp (asmir_bap_blocks_get bs n)@i) [] size




let x86_regs : var list =
  List.map (fun (n,t) -> Var.newvar n t)
    [
  (* 32 bit regs *)
  ("R_EBP", reg_32);
  ("R_ESP", reg_32);
  ("R_ESI", reg_32);
  ("R_EDI", reg_32);
  ("R_EIP", reg_32);
  ("R_EAX", reg_32);
  ("R_EBX", reg_32);
  ("R_ECX", reg_32);
  ("R_EDX", reg_32);
  ("EFLAGS", reg_32);

  (* condition flag bits *)
  ("R_CF", reg_1);
  ("R_PF", reg_1);
  ("R_AF", reg_1);
  ("R_ZF", reg_1);
  ("R_SF", reg_1);
  ("R_OF", reg_1);

  (* VEX left-overs from calc'ing condition flags *)
  ("R_CC_OP", reg_32);
  ("R_CC_DEP1", reg_32);
  ("R_CC_DEP2", reg_32);
  ("R_CC_NDEP", reg_32);

  (* more status flags *)
  ("R_DFLAG", reg_32);
  ("R_IDFLAG", reg_32);
  ("R_ACFLAG", reg_32);
  ("R_EMWARN", reg_32);
  ("R_LDT", reg_32); 
  ("R_GDT", reg_32); 

  (* segment regs *)
  ("R_CS", reg_16); 
  ("R_DS", reg_16); 
  ("R_ES", reg_16); 
  ("R_FS", reg_16); 
  ("R_GS", reg_16); 
  ("R_SS", reg_16); 

  (* floating point *)
  ("R_FTOP", reg_32);
  ("R_FPROUND", reg_32);
  ("R_FC3210", reg_32);
]


(* exectrace needs fixing if this is reg_64 *)
let x86_mem = Var.newvar "mem" (TMem(reg_32))


let arm_regs =
  List.map (fun n -> Var.newvar n reg_32)
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

let fold_memory_data f md acc =
  let size = Libasmir.memory_data_size md - 1 in
    foldn (fun a n ->
            let mcd = Libasmir.memory_data_get md n in
              f 
              (Libasmir.memory_cell_data_address mcd)
              (Libasmir.memory_cell_data_value mcd) 
              a)
      acc size

let get_rodata_assignments ?(prepend_to=[]) mem prog =
  let rodata = Libasmir.get_rodata prog in
  fold_memory_data
    (fun a v acc -> 
        let m_addr = Int(a, Reg 32) in
        let m_val = Int(Int64.of_int v, Reg 8) in
        Move(mem, Store(Var mem, m_addr, m_val, little_endian, Reg 8), []) :: acc)
    rodata prepend_to

(** Open a binary file for translation *)
let open_program filename =
  let prog = Libasmir.asmir_open_file filename in
    (* tell the GC how to free resources associated with prog *)
  Gc.finalise Libasmir.asmir_close prog;
  prog

(** Translate an entire Libasmir.asm_program_t into a Vine program *)
let asmprogram_to_bap ?(init_ro=false) asmp = 
  let bap_blocks = Libasmir.asmir_asmprogram_to_bap asmp in
  let arch = get_asmprogram_arch asmp in
  let g = gamma_for_arch arch in
  let ir = tr_bap_blocks_t g asmp bap_blocks in
  destroy_bap_blocks bap_blocks;
  if init_ro then
    let m = gamma_lookup g "$mem" in
    get_rodata_assignments ~prepend_to:ir m asmp
  else ir




(** Translate only one address of a  Libasmir.asm_program_t to Vine *)
let asm_addr_to_bap g prog addr =
  let block= Libasmir.asmir_addr_to_bap prog addr in
  let ir = tr_bap_block_t g prog block in
  destroy_bap_block block;
  ir


let asmprogram_to_bap_range ?(init_ro = false) asmp st en=
  let bap_blocks = Libasmir.asmir_asmprogram_range_to_bap asmp st en in
  let arch = get_asmprogram_arch asmp in
  let g = gamma_for_arch arch in
  let ir = tr_bap_blocks_t g asmp bap_blocks in
  destroy_bap_blocks bap_blocks;
  ir
