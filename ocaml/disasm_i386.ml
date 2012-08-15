(** Native lifter of x86 instructions to the BAP IL *)

open Int64
open Ast
open Ast_convenience
open BatPervasives
open Big_int_Z
open Big_int_convenience
open Type
open BatListFull

module VH=Var.VarHash

module D = Debug.Make(struct let name = "Disasm_i386" and default=`NoDebug end)
open D

(* 
   Note: In general, the function g is the get memory function.  The variable
   na refers to the next address or next instruction.

   To help understand this file, please refer to the
   Intel Instruction Set Reference. For consistency, any section numbers
   here are wrt Order Number: 253666-035US June 2010 and 253667-035US.


  The x86 instruction format is as follows:
   Instuction Prefixexs: 0-4bytes (1 byte per prefix)
   Opcode: 1 - 3 bytes.
   ModR/M: 1 optional byte
   SIB: 1 optional byte
   Displacement: 0,1,2, or 4 bytes.
   Immediate: 0,1,2, or 4 bytes

   ModR/M has the following format:
   7:6 Mod
   5:3 Reg or extra opcode bits
   2:0 R/M

   SIB:
   7:6 Scale
   5:3 Index
   2:0 Base


   In order to get the most common unspported opcodes, you can run something like:
   for f in bin/*; do BAP_DEBUG_MODULES=AsmirV ~/bap/trunk/utils/iltrans -bin $f ; done 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n

   To optimize for number of programs disassembled:
   for f in bin/*; do echo -n "$f "; BAP_DEBUG_MODULES=AsmirV iltrans -bin $f 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n  | wc -l; done | sort -n -k 2

*)

(* type segment = CS | SS | DS | ES | FS | GS *)

exception Disasm_i386_exception of string

type direction = Forward | Backward

type operand =
  | Oreg of int
  | Oaddr of Ast.exp
  | Oimm of int64 (* XXX: Should this be big_int? *)

type jumptarget =
  | Jabs of operand
  | Jrel of int64 * int64 (* next ins address, offset *)

type opcode =
  | Bswap of (typ * operand)
  | Retn of ((typ * operand) option) * bool (* bytes to release, far/near ret *)
  | Nop
  | Mov of typ * operand * operand * (Ast.exp option) (* dst, src, condition *)
  | Movs of typ
  | Movzx of typ * operand * typ * operand (* dsttyp, dst, srctyp, src *)
  | Movsx of typ * operand * typ * operand (* dsttyp, dst, srctyp, src *)
  | Movdq of typ * typ * operand * typ * operand * bool * string (* move type, dst type, dst op, src type, src op, aligned, name *)
  | Lea of operand * Ast.exp
  | Call of operand * int64 (* int64 is RA *)
  | Shift of binop_type * typ * operand * operand
  | Shiftd of binop_type * typ * operand * operand * operand
  | Rotate of binop_type * typ * operand * operand * bool (* left or right, type, src/dest op, shift op, use carry flag *)
  | Bt of typ * operand * operand
  | Bs of typ * operand * operand * direction
  | Jump of jumptarget
  | Jcc of jumptarget * Ast.exp
  | Setcc of typ * operand * Ast.exp
  | Hlt
  | Cmps of typ
  | Scas of typ
  | Stos of typ
  | Push of typ * operand
  | Pop of typ * operand
  | Pushf of typ
  | Popf of typ
  | Sahf
  | Lahf
  | Add of (typ * operand * operand)
  | Adc of (typ * operand * operand)
  | Inc of typ * operand
  | Dec of typ * operand
  | Sub of (typ * operand * operand)
  | Sbb of (typ * operand * operand)
  | Cmp of (typ * operand * operand)
  | Cmpxchg of (typ * operand * operand)
  | Cmpxchg8b of operand
  | Xadd of (typ * operand * operand)
  | Xchg of (typ * operand * operand)
  | And of (typ * operand * operand)
  | Or of (typ * operand * operand)
  | Xor of (typ * operand * operand)
  | Pxor of (typ * operand * operand)
  | Test of (typ * operand * operand)
  | Ptest of (typ * operand * operand)
  | Not of (typ * operand)
  | Neg of (typ * operand)
  | Mul of (typ * operand) (* typ, src *)
  | Imul of typ * (bool * operand) * operand * operand (* typ, (true if one operand form, dst operand), src1, src2 *)
  | Div of typ * operand (* typ, src *)
  | Idiv of typ * operand (* typ, src *)
  | Cld
  | Rdtsc
  | Cpuid
  | Stmxcsr of operand
  | Ldmxcsr of operand
  | Fnstcw of operand
  | Fldcw of operand
  | Fld of operand
  | Fst of (operand * bool)
  | Pmovmskb of (typ * operand * operand)
  | Pcmpeq of (typ * typ * operand * operand)
  | Palignr of (typ * operand * operand * operand)
  | Pcmpistri of (typ * operand * operand * operand)
  | Pshufd of operand * operand * operand
  | Leave of typ
  | Interrupt of operand
  | Sysenter

(* prefix names *)
let pref_lock = 0xf0
and repnz = 0xf2
and repz = 0xf3
and hint_bnt = 0x2e
and hint_bt = 0x3e
and pref_cs = 0x2e
and pref_ss = 0x36
and pref_ds = 0x3e
and pref_es = 0x26
and pref_fs = 0x64
and pref_gs = 0x65
and pref_opsize = 0x66
and pref_addrsize = 0x67

type prefix = {
  opsize   : typ;
  mopsize  : typ;
  repeat   : bool;
  nrepeat  : bool;
  addrsize_override : bool;
  opsize_override : bool;
  (* add more as needed *)
}

(** disfailwith is a non-fatal disassembly exception. *)
let disfailwith s = raise (Disasm_i386_exception s)

let unimplemented s  = disfailwith ("disasm_i386: unimplemented feature: "^s)

let (&) = (land)
and (>>) = (lsr)
and (<<) = (lsl)
let ite t b e1 e2 =
  exp_ite ~t b e1 e2

(* register widths *)
let r1 = Ast.reg_1
let r4 = Reg 4
let r8 = Ast.reg_8
let r16 = Ast.reg_16
let r32 = Ast.reg_32
let addr_t = r32
let r64 = Ast.reg_64
let r128 = Reg 128
let xmm_t = Reg 128
let st_t = Reg 80

(** Only use this for registers, not temporaries *)
let nv = Var.newvar
let nt = Disasm_temp.nt

(* registers *)

let ebp = nv "R_EBP" r32
and esp = nv "R_ESP" r32
and esi = nv "R_ESI" r32
and edi = nv "R_EDI" r32
and eip = nv "R_EIP" r32 (* why is eip in here? *)
and eax = nv "R_EAX" r32
and ebx = nv "R_EBX" r32
and ecx = nv "R_ECX" r32
and edx = nv "R_EDX" r32
and eflags = nv "EFLAGS" r32 (* why is eflags in here? *)
  (* condition flag bits *)
and cf = nv "R_CF" r1
and pf = nv "R_PF" r1
and af = nv "R_AF" r1
and zf = nv "R_ZF" r1
and sf = nv "R_SF" r1
and oF = nv "R_OF" r1

and dflag = nv "R_DFLAG" r32 (* 1 if DF=0 or -1 if DF=1 *)

and fs_base = nv "R_FS_BASE" r32
and gs_base = nv "R_GS_BASE" r32

and fpu_ctrl = nv "R_FPU_CONTROL" r16
and mxcsr = nv "R_MXCSR" r32

let xmms = Array.init 8 (fun i -> nv (Printf.sprintf "R_XMM%d" i) xmm_t)

(* floating point registers *)
let st = Array.init 8 (fun i -> nv (Printf.sprintf "R_ST%d" i) st_t)

let regs : var list =
  ebp::esp::esi::edi::eip::eax::ebx::ecx::edx::eflags::cf::pf::af::zf::sf::oF::dflag::fs_base::gs_base::fpu_ctrl::mxcsr::
  List.map (fun (n,t) -> Var.newvar n t)
    [

  (* VEX left-overs from calc'ing condition flags *)
  ("R_CC_OP", reg_32);
  ("R_CC_DEP1", reg_32);
  ("R_CC_DEP2", reg_32);
  ("R_CC_NDEP", reg_32);

  (* status flags/misc *)
  ("R_IDFLAG", reg_32);
  ("R_ACFLAG", reg_32);
  ("R_EMWARN", reg_32);
  ("R_LDT", reg_32);
  ("R_GDT", reg_32);
  ("R_IP_AT_SYSCALL", reg_32);

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
    @ Array.to_list xmms
    @ Array.to_list st   (* floating point *)

let o_eax = Oreg 0
and o_ecx = Oreg 1
and o_edx = Oreg 2
and o_ebx = Oreg 3
and o_esp = Oreg 4
and o_ebp = Oreg 5
and o_esi = Oreg 6
and o_edi = Oreg 7

let esp_e = Var esp
and ebp_e = Var ebp
and esi_e = Var esi
and edi_e = Var edi
and ecx_e = Var ecx
and eax_e = Var eax

let mem = nv "mem" (TMem(r32))
let mem_e = Var mem
and cf_e = Var cf
and pf_e = Var pf
and af_e = Var af
and zf_e = Var zf
and sf_e = Var sf
and of_e = Var oF

and dflag_e = Var dflag

(* BAP's dflag is 1 or -1, indicating forward or backwards.

   x86 uses values 0 and 1 for forward and backwards. *)
let x86_dflag_e =
  ite reg_1 (dflag_e ==* Int(bi1, reg_32)) exp_false exp_true

let esiaddr = Oaddr esi_e
and ediaddr = Oaddr edi_e

let seg_cs = None
and seg_ss = None
and seg_ds = None
and seg_es = None
and seg_fs = Some fs_base
and seg_gs = Some gs_base

(* eflags *)
let dflag_to_bap e =
  ite reg_32 (e ==* exp_false) (Int(bi1, reg_32)) (Int(bim1, reg_32))

let bap_to_eflags =
  let undefined d = Unknown(Printf.sprintf "Undefined EFLAGS bit %d" d, reg_1) in
  let unmodeled s = Unknown("Unmodeled EFLAGS bit " ^ s, reg_1) in
  undefined 31                  (* 31 *)
  :: undefined 30               (* 30 *)
  :: undefined 29               (* 29 *)
  :: undefined 28               (* 28 *)
  :: undefined 27               (* 27 *)
  :: undefined 26               (* 26 *)
  :: undefined 25               (* 25 *)
  :: undefined 24               (* 24 *)
  :: undefined 23               (* 23 *)
  :: undefined 22               (* 22 *)
  :: unmodeled "ID"             (* 21 *)
  :: unmodeled "VIP"            (* 20 *)
  :: unmodeled "VIF"            (* 19 *)
  :: unmodeled "AC"             (* 18 *)
  :: unmodeled "VM"             (* 17 *)
  :: unmodeled "RF"             (* 16 *)
  :: undefined 15               (* 15 *)
  :: unmodeled "NT"             (* 14 *)
  :: unmodeled "IOPL1"          (* 13 *)
  :: unmodeled "IOPL2"          (* 12 *)
  :: of_e                       (* 11 *)
  :: x86_dflag_e                (* 10 *)
  :: unmodeled "IF"             (*  9 *)
  :: unmodeled "TF"             (*  8 *)
  :: sf_e                       (*  7 *)
  :: zf_e                       (*  6 *)
  :: undefined 5                (*  5 *)
  :: af_e                       (*  4 *)
  :: undefined 3                (*  3 *)
  :: pf_e                       (*  2 *)
  :: undefined 1                (*  1 *)
  :: cf_e                       (*  0 *)
  :: []
let bap_to_flags = BatList.drop 16 bap_to_eflags
let bap_to_lflags = BatList.drop 8 bap_to_flags

let eflags_e = BatList.reduce (++*) bap_to_eflags
and flags_e = BatList.reduce (++*) bap_to_flags
and lflags_e = BatList.reduce (++*) bap_to_lflags

let eflags_to_bap =
  let assn v = Some(v, Util.id) in
  None                          (* 31 *)
  :: None                       (* 30 *)
  :: None                       (* 29 *)
  :: None                       (* 28 *)
  :: None                       (* 27 *)
  :: None                       (* 26 *)
  :: None                       (* 25 *)
  :: None                       (* 24 *)
  :: None                       (* 23 *)
  :: None                       (* 22 *)
  :: None                       (* 21 *)
  :: None                       (* 20 *)
  :: None                       (* 19 *)
  :: None                       (* 18 *)
  :: None                       (* 17 *)
  :: None                       (* 16 *)
  :: None                       (* 15 *)
  :: None                       (* 14 *)
  :: None                       (* 13 *)
  :: None                       (* 12 *)
  :: assn oF                    (* 11 *)
  :: Some(dflag, dflag_to_bap)  (* 10 *)
  :: None                       (* 09 *)
  :: None                       (* 08 *)
  :: assn sf                    (* 07 *)
  :: assn zf                    (* 06 *)
  :: None                       (* 05 *)
  :: assn af                    (* 04 *)
  :: None                       (* 03 *)
  :: assn pf                    (* 02 *)
  :: None                       (* 01 *)
  :: assn cf                    (* 00 *)
  :: []
let flags_to_bap = BatList.drop 16 eflags_to_bap
let lflags_to_bap = BatList.drop 8 flags_to_bap
(* A list of functions for assigning each bit in eflags *)
let assns_eflags_to_bap =
  List.map
    (function
      | None -> (fun e -> [])
      | Some (v,f) -> (fun e -> [Move(v,f e,[])]))
    eflags_to_bap
let assns_flags_to_bap = BatList.drop 16 assns_eflags_to_bap
let assns_lflags_to_bap = BatList.drop 8 assns_flags_to_bap


(* exp helpers *)

let loadm m t a =
  Load(Var m, a, little_endian, t)

let load_s s t a = match s with
  | None -> Load(mem_e, a, little_endian, t)
  | Some v -> Load(mem_e, Var v +* a, little_endian, t)

let l32 i = Int(Arithmetic.to_big_int (big_int_of_int64 i,r32), r32)
let l16 i = Int(Arithmetic.to_big_int (big_int_of_int64 i,r16), r16)
let lt i t = Int(Arithmetic.to_big_int (big_int_of_int64 i,t), t)

let i32 i = Int(biconst i, r32)
let it i t = Int(biconst i, t)

(* converts a register number to the corresponding 32bit register variable *)
let bits2reg32= function
  | 0 -> eax
  | 1 -> ecx
  | 2 -> edx
  | 3 -> ebx
  | 4 -> esp
  | 5 -> ebp
  | 6 -> esi
  | 7 -> edi
  | _ -> failwith "bits2reg32 takes 3 bits"

let bits2xmm b = xmms.(b)

and reg2bits r =
  let (i,_) = BatList.findi (fun _ x -> x == r) [eax; ecx; edx; ebx; esp; ebp; esi; edi] in
  i

let bits2xmme b = Var(bits2xmm b)

let bits2reg64e b =
  cast_low r64 (bits2xmme b)

let bits2reg32e b = Var(bits2reg32 b)

let bits2reg16e b =
  cast_low r16 (bits2reg32e b)

let bits2reg8e b =
  if b < 4 then
    cast_low r8 (bits2reg32e b)
  else
    cast_high r8 (cast_low r16 (bits2reg32e (b land 3)))

let reg2xmm r =
  bits2xmm (reg2bits r)

(* These aren't used by Disasm_i386, but might be useful to external
   users. *)
let subregs =
  let hi r = (reg2bits r) + 4 in
  (eax, "R_AL", bits2reg8e (reg2bits eax))
  :: (ecx, "R_CL", bits2reg8e (reg2bits ecx))
  :: (edx, "R_DL", bits2reg8e (reg2bits edx))
  :: (ebx, "R_BL", bits2reg8e (reg2bits ebx))
  :: (eax, "R_AH", bits2reg8e (hi eax))
  :: (ecx, "R_CH", bits2reg8e (hi ecx))
  :: (edx, "R_DH", bits2reg8e (hi edx))
  :: (ebx, "R_BH", bits2reg8e (hi ebx))
  :: (eax, "R_AX", bits2reg16e (reg2bits eax))
  :: (ecx, "R_CX", bits2reg16e (reg2bits ecx))
  :: (edx, "R_DX", bits2reg16e (reg2bits edx))
  :: (ebx, "R_BX", bits2reg16e (reg2bits ebx))
  :: []

let subregs_find =
  let h = VH.create 10 in
  let () = List.iter (fun ((fr,_,_) as t) -> VH.add h fr t) subregs in
  VH.find_all h

(* effective addresses for 16-bit addressing *)
let eaddr16 = function
  (* R/M byte *)
  | 0 -> (Var ebx) +* (Var esi)
  | 1 -> (Var ebx) +* (Var edi)
  | 2 -> (Var ebp) +* (Var esi)
  | 3 -> (Var ebp) +* (Var edi)
  | 4 -> Var esi
  | 5 -> Var edi
  | 6 -> Var ebp
  | 7 -> Var ebx
  | _ -> disfailwith "eaddr16 takes only 0-7"

let eaddr16e b = cast_low r16 (eaddr16 b)

let ah_e = bits2reg8e 4
let ch_e = bits2reg8e 5
let dh_e = bits2reg8e 6
let bh_e = bits2reg8e 7

module ToIR = struct

(* stmt helpers *)

let move v e =
  Move(v, e, [])

let store_s s t a e = match s with
  | None -> move mem (Store(mem_e, a, e, little_endian, t))
  | Some v -> move mem (Store(mem_e, Var v +* a, e, little_endian, t))

let storem m t a e =
  move m (Store(Var m, a, e, little_endian, t))

let op2e_s ss t = function
  | Oreg r when t = r128 -> bits2xmme r
  | Oreg r when t = r64 -> bits2reg64e r
  | Oreg r when t = r32 -> bits2reg32e r
  | Oreg r when t = r16 -> bits2reg16e r
  | Oreg r when t = r8 -> bits2reg8e r
  | Oreg r -> unimplemented "unknown register"
  | Oaddr e -> load_s ss t e
  | Oimm i -> Int(Arithmetic.to_big_int (big_int_of_int64 i,t), t)

let assn_s s t v e =
  (* Assign to some bits of v, starting at bit off, while preserving the other bits *)
  let sub_assn ?(off=0) t v e =
    let concat_exps = ref [] in
    let bits = Typecheck.bits_of_width (Var.typ v) in
    let assnbits = Typecheck.bits_of_width t in

    (* Add the upper preserved bits, if any *)
    let ubh = bi (bits-1) and ubl = bi (assnbits+off) in
    if ubh > ubl then
      concat_exps := extract ubh ubl (Var v)::!concat_exps;

    (* Add e *)
    concat_exps := e::!concat_exps;

    (* Add the lower preserved bits, if any *)
    let lbh = bi (off-1) and lbl = bi0 in
    if lbh > lbl then
      concat_exps := extract lbh lbl (Var v)::!concat_exps;

    let final_e = BatList.reduce (fun big_e e -> Ast_convenience.concat e big_e) !concat_exps in
    move v final_e
  in
  match v with
  | Oreg r when t = r128 -> move (bits2xmm r) e
  | Oreg r when t = r64 ->
    let v = bits2xmm r in
    sub_assn t v e
  | Oreg r when t = r32 -> move (bits2reg32 r) e
  | Oreg r when t = r16 ->
    let v = bits2reg32 r in
    sub_assn t v e
  | Oreg r when t = r8 && r < 4 ->
    let v = bits2reg32 r in
    sub_assn t v e
  | Oreg r when t = r8 ->
    let v = bits2reg32 (r land 3) in
    sub_assn ~off:8 t v e
  | Oreg _ -> unimplemented "assignment to sub registers"
  | Oaddr a -> store_s s t a e
  | Oimm _ -> disfailwith "disasm_i386: Can't assign to an immediate value"

(* Double width operands, as used by multiplication and division *)
let op_dbl = function
  | Reg 8 -> [reg_16, o_eax]
  | Reg 16 -> [reg_16, o_edx; reg_16, o_eax]
  | Reg 32 -> [reg_32, o_edx; reg_32, o_eax]
  | _ -> disfailwith "op_dbl only defined for Reg 8, 16, and 32"

(* Return an expression for a double-width operand, as used by the div
   instruction. *)
let op2e_dbl_s ss t =
  let cf (ct, o) = op2e_s ss ct o in
  let ol = List.map cf (op_dbl t) in
  List.fold_left
    (fun bige little -> bige ++* little)
    (List.hd ol)
    (List.tl ol)

(* Double width assignments, as used by multiplication *)
let assn_dbl_s s t e = match op_dbl t with
  | (t,o) :: [] -> [assn_s s t o e], op2e_s s t o
  | l ->
    let tmp = nt "t" (Reg (Typecheck.bits_of_width t * 2)) in
    let f (stmts, off) (ct, o) = 
      let newoff = off + Typecheck.bits_of_width ct in
      assn_s s ct o (extract (biconst (newoff-1)) (biconst off) (Var tmp))::stmts, newoff
    in
    List.rev (fst (List.fold_left f ([move tmp e], 0) (List.rev l))), Var tmp

(* A function for computing the target of jumps. *)
let compute_jump_target s = function
  | Jabs o -> op2e_s s r32 o
  | Jrel (na,offset) ->
    let na = biconst64 na in
    let offset = biconst64 offset in
    let i,t = Arithmetic.binop PLUS (na,reg_32) (offset,reg_32) in
    Int(i,t)
let jump_target = compute_jump_target

let bytes_of_width = Typecheck.bytes_of_width
let bits_of_width = Typecheck.bits_of_width

let string_incr t v =
  if t = r8 then
    move v (Var v +* dflag_e)
  else
    move v (Var v +* (dflag_e ** i32(bytes_of_width t)))

let rep_wrap ?check_zf ~addr ~next stmts =
  let endstmt = match check_zf with
    | None -> Jmp(l32 addr, [])
    | Some x when x = repz ->
      CJmp(zf_e, l32 addr, l32 next, [])
    | Some x when x = repnz ->
      CJmp(zf_e, l32 next, l32 addr, [])
    | _ -> failwith "invalid value for ?check_zf"
  in
  cjmp (ecx_e ==* l32 0L) (l32 next)
  @ stmts
  @ move ecx (ecx_e -* i32 1)
  :: cjmp (ecx_e ==* l32 0L) (l32 next)
  @ [endstmt]

let reta = [StrAttr "ret"]
and calla = [StrAttr "call"]

let compute_sf result = cast_high r1 result
let compute_zf t result = Int(bi0, t) ==* result
let compute_pf t r =
  (* extra parens do not change semantics but do make it pretty print nicer *)
  exp_not (cast_low r1 ((((((((r >>* it 7 t) ^* (r >>* it 6 t)) ^* (r >>* it 5 t)) ^* (r >>* it 4 t)) ^* (r >>* it 3 t)) ^* (r >>* it 2 t)) ^* (r >>* it 1 t)) ^* r))

let set_sf r = move sf (compute_sf r)
let set_zf t r = move zf (compute_zf t r)
let set_pf t r = move pf (compute_pf t r)

let set_pszf t r =
  [set_pf t r;
   set_sf r;
   set_zf t r]

(* Adjust flag

   AF is set when there is a carry to or borrow from bit 4 (starting
   at 0), when considering unsigned operands. Let X_i denote bit i of
   value X.  Note that in addition, r_4 = c + [(op1_4 + op2_4) mod 2],
   where c is the carry bit from the lower four bits. Since AF = c,
   and we want to know the value of AF, we can rewrite as AF = c = r_4
   - [(op1_4 + op2_4) mod 2]. Noting that addition and subtraction mod
   2 is just xor, we can simplify to AF = r_4 xor op1_4 xor op2_4.
*)

(* Helper functions to set flags for adding *)
let set_aopszf_add t s1 s2 r =
  let bit4 = it (1 lsl 4) t in
  move af (bit4 ==* (bit4 &* ((r ^* s1) ^* s2)))
  ::move oF (cast_high r1 ((s1 =* s2) &* (s1 ^* r)))
  ::set_pszf t r

let set_flags_add t s1 s2 r =
  move cf (r <* s1)
  ::set_aopszf_add t s1 s2 r

(* Helper functions to set flags for subtracting *)
let set_apszf_sub t s1 s2 r =
  let bit4 = it (1 lsl 4) t in
  move af (bit4 ==* ((bit4 &* ((r ^* s1) ^* s2))))
  ::set_pszf t r

let set_aopszf_sub t s1 s2 r =
  move oF (cast_high r1 ((s1 ^* s2) &* (s1 ^* r)))
  ::set_apszf_sub t s1 s2 r

let set_flags_sub t s1 s2 r =
  move cf (s2 >* s1)
  ::set_aopszf_sub t s1 s2 r


let rec to_ir addr next ss pref =
  let load = load_s ss (* Need to change this if we want seg_ds <> None *)
  and op2e = op2e_s ss
  and op2e_dbl = op2e_dbl_s ss
  and store = store_s ss
  and assn = assn_s ss 
  and assn_dbl = assn_dbl_s ss in
  function
  | Nop -> []
  | Bswap(t, op) ->
    let e = match t with
      | Reg 32 ->
        reverse_bytes (op2e t op)
      | Reg 16 ->
        unknown t "result of bswap is undefined for 16 bit operand"
      | _ -> disfailwith "bswap: Expected 16 or 32 bit type"
    in
    [assn t op e]
  | Retn (op, far_ret) when pref = [] ->
    let temp = nt "ra" r32 in
    let load_stmt = if far_ret 
      then (* TODO Mess with segment selectors here *)  
    	unimplemented "long retn not supported"  
      else move temp (load_s seg_ss r32 esp_e)
    in
    let esp_stmts = 
      move esp (esp_e +* (i32 (bytes_of_width r32)))::
	(match op with 
	| None -> []
	| Some(t, src) -> 
	  [move esp (esp_e +* (op2e t src))]
      ) in
      load_stmt::
      esp_stmts@
      [Jmp(Var temp, [StrAttr "ret"])]
  | Mov(t, dst, src, condition) when pref = [] || pref = [pref_addrsize] ->
    let c_src = (match condition with 
      | None -> op2e t src
      | Some(c) -> ite t c (op2e t src) (op2e t dst))
    in
      [assn t dst c_src]
  | Movs(Reg bits as t) ->
      let stmts =
	store_s seg_es t edi_e (load_s seg_es t esi_e)
	:: string_incr t esi
	:: string_incr t edi
	:: []
      in
      if pref = [] then
	stmts
      else if pref = [repz] || pref = [repnz] then
        (* movs has only rep instruction others just considered to be rep *)
	rep_wrap ~addr ~next stmts
      else
	unimplemented "unsupported prefix for movs"
  | Movzx(t, dst, ts, src) when pref = [] ->
    [assn t dst (cast_unsigned t (op2e ts src))]
  | Movsx(t, dst, ts, src) when pref = [] ->
    [assn t dst (cast_signed t (op2e ts src))]
  | Movdq(t, td, d, ts, s, align, _name) ->
    let (s, al) = match s with
      | Oreg i -> op2e ts s, []
      | Oaddr a -> op2e ts s, [a]
      | Oimm _ -> disfailwith "invalid"
    in
    let b = bits_of_width in
    let s =
      if b ts < b t then cast_unsigned t s
      else if b ts > b t then cast_low t s
      else s
    in
    (* s is now of type t, but we need it as type td *)
    let s =
      if b t < b td then cast_unsigned td s
      else if b t > b td then cast_low td s
      else s
    in
    let (d, al) = match d with
      | Oreg i -> assn td d s, al
	(* let r = op2e t d in *)
	(* move r s, al *)
      | Oaddr a -> assn td d s, a::al
      | Oimm _ -> disfailwith "invalid"
    in
    let al =
      if align then
	List.map (fun a -> Assert( (a &* i32 15) ==* i32 0, [])) (al)
      else []
    in
    d::al
  | Pcmpeq (t,elet,dst,src) ->
      let ncmps = (bits_of_width t) / (bits_of_width elet) in
      let elebits = bits_of_width elet in
      let src = match src with
        | Oreg i -> op2e t src
        | Oaddr a -> load t a
        | Oimm _ -> disfailwith "invalid"
      in
      let compare_region i =
        let byte1 = Extract(biconst (i*elebits-1), biconst ((i-1)*elebits), src) in
        let byte2 = Extract(biconst (i*elebits-1), biconst ((i-1)*elebits), op2e t dst) in
        let tmp = nt ("t" ^ string_of_int i) elet in
        Var tmp, move tmp (Ite(byte1 ==* byte2, lt (-1L) elet, lt 0L elet))
      in
      let indices = BatList.init ncmps (fun i -> i + 1) in (* list 1-nbytes *)
      let comparisons = List.map compare_region indices in
      let temps, cmps = List.split comparisons in
      let temps = List.rev temps in
        (* could also be done with shifts *)
      let store_back = List.fold_left (fun acc i -> Concat(acc,i)) (List.hd temps) (List.tl temps) in
      cmps @ [assn t dst store_back]
  | Pmovmskb (t,dst,src) ->
      let nbytes = bytes_of_width t in
      let src = match src with
        | Oreg i -> op2e t src
        | _ -> disfailwith "invalid operand"
      in
      let get_bit i = Extract(biconst (i*8-1), biconst (i*8-1), src) in
      let byte_indices = BatList.init nbytes (fun i -> i + 1) in (* list 1-nbytes *)
      let all_bits = List.map get_bit byte_indices in
      let all_bits = List.rev all_bits in
        (* could also be done with shifts *)
      let padt = Reg(32 - nbytes) in
      let or_together_bits = List.fold_left (fun acc i -> Concat(acc,i)) (it 0 padt) all_bits in
      [assn r32 dst or_together_bits]
  | Palignr (t,dst,src,imm) ->
      let dst_e = op2e t dst in
      let src_e = op2e t src in
      let imm = op2e t imm in
      let concat = dst_e ++* src_e in
      let t_concat = Typecheck.infer_ast ~check:false concat in
      let shift = concat >>* (cast_unsigned t_concat (imm <<* (it 3 t))) in
      let high, low = match t with
        | Reg 128 -> biconst 127, bi0
        | Reg 64 -> biconst 63, bi0
        | _ -> disfailwith "impossible: used non 64/128-bit operand in palignr"
      in
      let result = Extract (high, low, shift) in
      let addresses = List.fold_left (fun acc -> function Oaddr a -> a::acc | _ -> acc) [] [src;dst] in
      List.map (fun addr -> Assert( (addr &* i32 15) ==* i32 0, [])) addresses
      @ [assn t dst result]
  | Pcmpistri(t,xmm1,xmm2m128,imm) ->
    (* All bytes and bits are numbered with zero being the least
       significant. This includes strings! *)
    (* NOTE: Strings are backwards, at least when they are in
       registers.  This doesn't seem to be documented in the Intel
       manual.  This means that the NULL byte comes before the
       string. *)
    let xmm1_e = op2e t xmm1 in
    let xmm2m128_e = op2e t xmm2m128 in
      (* only handles imm == 0xc *)
      assert (imm = Oimm 0xcL);
      (* Is there a substring starting at i? *)
      let get_bit i =
        let get_indices base index =
          let t = (base+index)*8 in
          biconst (t+7), biconst t
        in
        let fold_cmp acc j =
          (* Compare the jth bit of the substring *)
          let xmm1_e_byte = 
            let u, l = get_indices 0 j in
            Extract(u, l, xmm1_e)
          in
          let xmm2m128_e_byte =
            let u, l = get_indices i j in
            Extract(u, l, xmm2m128_e)
          in
          Ite(BinOp(EQ, Int(bi0, reg_8), xmm1_e_byte),
            exp_true,
            Ite(BinOp(NEQ, xmm2m128_e_byte, xmm1_e_byte),
              exp_false,
              acc
            )
          )
        in
        (fold fold_cmp exp_true ((15-i)---0))
      in
      let contains_null e =
        fold (fun acc i ->
          Ite(BinOp(EQ, Extract(biconst (i*8+7), biconst (i*8), e), Int(bi0, reg_8)),
              exp_true,
              acc
          )
        ) exp_false (0--15)
      in
      let msb e =
        fold (fun acc i ->
          Ite(BinOp(EQ, exp_true, Extract(biconst i, biconst i, e)),
              Int(biconst i, reg_32),
              acc
          )
        ) (Int(biconst 16, reg_32)) (15---0)
      in
      let bits = map get_bit (15---0) in
      let res_e = concat_explist bits in
      let int_res_2 = nt "IntRes2" reg_16 in
      move int_res_2 res_e
      (* This is supposed to be lsb, but lsb works in practice.
         It's like we are generating IntRes in reverse. *)
      :: move ecx (msb (Var int_res_2))
      :: move cf (BinOp(NEQ, (Var int_res_2), Int(bi0, reg_16)))
      :: move zf (contains_null xmm2m128_e)
      :: move sf (contains_null xmm1_e)
      :: move oF (Extract(bi0, bi0, (Var int_res_2)))
      :: move af (Int(bi0, reg_1))
      :: move pf (Int(bi0, reg_1))
      :: []
  | Pshufd (dst, src, imm) ->
    let t = r128 in (* pshufd is only defined for 128-bits *)
      let src_e = op2e t src in
      let imm_e = op2e t imm in
      let get_dword prev_dwords ndword =
        let high = 2 * ndword + 1 |> biconst in
        let low = 2 * ndword |> biconst in
        let encoding = cast_unsigned t (Extract (high, low, imm_e)) in
        let shift = encoding ** (it 32 t) in
        let dword = src_e >>* shift in
        let dword = cast_low r32 dword in
        match prev_dwords with
          | None ->
              Some dword
          | Some dwords ->
              Some (Concat(dwords, dword))
        in
      let dst_dwords = fold get_dword None (0--3) in
      (match dst_dwords with
         | Some dwords ->
             [assn t dst dwords]
         | None ->
             disfailwith "failed to read dwords for pshufd"
      )
  | Pxor(t, o1, o2) ->
    [assn t o1 (op2e t o1 ^* op2e t o2)]
    (* Pxor does not set any flags! *)
  | Lea(r, a) when pref = [] ->
    [assn r32 r a]
  | Call(o1, ra) when pref = [] ->
    (* If o1 is an immediate, we should syntactically have Jump(imm)
       so that the CFG algorithm knows where the jump goes.  Otherwise
       it will point to BB_Indirect.

       Otherwise, we should evaluate the operand before decrementing esp.
       (This really only matters when esp is the base register of a memory
       lookup. *)
    let target = op2e r32 o1 in
    (match o1 with
    | Oimm _ ->
      let t = nt "target" r32 in
      [move t target;
       move esp (esp_e -* i32 4);
       store_s None r32 esp_e (l32 ra);
       Jmp(target, calla)]
    | _ ->
      let t = nt "target" r32 in
      [move t target;
       move esp (esp_e -* i32 4);
       store_s None r32 esp_e (l32 ra);
       Jmp(Var t, calla)])
  | Jump(o) ->
    [Jmp(jump_target ss o, [])]
  | Jcc(o, c) ->
    cjmp c (jump_target ss o)
  | Setcc(t, o1, c) ->
    [assn t o1 (cast_unsigned t c)]
  | Shift(st, s, dst, shift) -> 
    assert (List.mem s [r8; r16; r32]);
    let origCOUNT, origDEST = nt "origCOUNT" s, nt "origDEST" s
    and size = it (bits_of_width s) s
    and s_f = match st with LSHIFT -> (<<*) | RSHIFT -> (>>*) 
      | ARSHIFT -> (>>>*) | _ -> disfailwith "invalid shift type"
    and count = (op2e s shift) &* (it 31 s)
    and dste = op2e s dst in
    let ifzero = ite r1 (Var origCOUNT ==* (it 0 s))
    and new_of = match st with
      | LSHIFT -> (cast_high r1 dste) ^* cf_e
      | RSHIFT -> cast_high r1 (Var origDEST)
      | ARSHIFT -> exp_false
      | _ -> disfailwith "imposible"
    in
    let unk_of = Unknown("OF undefined after shift", r1) in
    let new_cf = 
      (* undefined for SHL and SHR instructions where the count is greater than 
	 or equal to the size (in bits) of the destination operand *)
      match st with
      | LSHIFT -> cast_low r1 (Var origDEST >>* (size -* Var origCOUNT))
      | RSHIFT | ARSHIFT ->
	cast_high r1 (Var origDEST <<* (size -* Var origCOUNT))
      | _ -> failwith "impossible"
    in
    [move origDEST dste;
     move origCOUNT count;
     assn s dst (s_f dste count);
     move cf (ifzero cf_e new_cf);
     move oF (ifzero of_e (ite r1 (Var origCOUNT ==* (it 1 s)) new_of unk_of));
     move sf (ifzero sf_e (compute_sf dste));
     move zf (ifzero zf_e (compute_zf s dste));
     move pf (ifzero pf_e (compute_pf s dste));
     move af (ifzero af_e (Unknown("AF undefined after shift", r1)))
    ]
  | Shiftd(st, s, dst, fill, count) ->
      let origDEST, origCOUNT = nt "origDEST" s, nt "origCOUNT" s in
      let e_dst = op2e s dst in
      let e_fill = op2e s fill in
      (* count mod 32 *)
      let e_count = (op2e s count) &* (it 31 s) in
      let size = it (bits_of_width s) s in
      let new_cf =  match st with
	| LSHIFT -> cast_low r1 (Var origDEST >>* (size -* Var origCOUNT))
	| RSHIFT -> cast_high r1 (Var origDEST <<* (size -* Var origCOUNT))
	| _ -> disfailwith "imposible" in
      let ifzero = ite r1 ((Var origCOUNT) ==* (it 0 s)) in
      let new_of = cast_high r1 (Var origDEST) ^* cast_high r1 e_dst in
      let unk_of = 
	Unknown ("OF undefined after shiftd of more then 1 bit", r1) in
      let ret1 = match st with
	| LSHIFT -> e_fill >>* (size -* Var origCOUNT)
	| RSHIFT -> e_fill <<* (size -* Var origCOUNT)
	| _ -> disfailwith "imposible" in
      let ret2 = match st with
	| LSHIFT -> e_dst <<* Var origCOUNT
	| RSHIFT -> e_dst >>* Var origCOUNT
	| _ -> disfailwith "imposible" in
      let result = ret1 |* ret2 in
      (* SWXXX If shift is greater than the operand size, dst and
	 flags are undefined *)
      [
        move origDEST e_dst;
	move origCOUNT e_count;
        assn s dst result;
        move cf (ifzero cf_e new_cf);
	(* For a 1-bit shift, the OF flag is set if a sign change occurred; 
	   otherwise, it is cleared. For shifts greater than 1 bit, the OF flag 
	   is undefined. *)
        move oF (ifzero of_e (ite r1 ((Var origCOUNT) ==* i32 1) new_of unk_of));
        move sf (ifzero sf_e (compute_sf e_dst));
        move zf (ifzero zf_e (compute_zf s e_dst));
        move pf (ifzero pf_e (compute_pf s e_dst));
        move af (ifzero af_e (Unknown ("AF undefined after shiftd", r1)))
      ]
  | Rotate(rt, s, dst, shift, use_cf) ->
    (* SWXXX implement use_cf *)
    if use_cf then unimplemented "rotate use_vf";
    let origCOUNT = nt "origCOUNT" s in
    let e_dst = op2e s dst in
    let e_shift = op2e s shift &* it 31 s in
    let size = it (bits_of_width s) s in
    let new_cf = match rt with
      | LSHIFT -> cast_low r1 e_dst
      | RSHIFT -> cast_high r1 e_dst 
      | _ -> disfailwith "imposible" in
    let new_of = match rt with
      | LSHIFT -> cf_e ^* cast_high r1 e_dst
      | RSHIFT -> cast_high r1 e_dst ^* cast_high r1 (e_dst <<* it 1 s)
      | _ -> disfailwith "imposible" in
    let unk_of =
      Unknown ("OF undefined after rotate of more then 1 bit", r1) in
    let ifzero = ite r1 (Var origCOUNT ==* it 0 s) in
    let ret1 = match rt with
    	| LSHIFT -> e_dst <<* Var origCOUNT
    	| RSHIFT -> e_dst >>* Var origCOUNT
    	| _ -> disfailwith "imposible" in
    let ret2 = match rt with
    	| LSHIFT -> e_dst >>* (size -* Var origCOUNT)
    	| RSHIFT -> e_dst <<* (size -* Var origCOUNT)
    	| _ -> disfailwith "imposible" in
    let result = ret1 |* ret2 in
    [
      move origCOUNT e_shift;
      assn s dst result;
      (* cf must be set before of *)
      move cf (ifzero cf_e new_cf);
      move oF (ifzero of_e (ite r1 (Var origCOUNT ==* it 1 s) new_of unk_of));
    ]
  | Bt(t, bitoffset, bitbase) ->
      let offset = op2e t bitoffset in
      let value, shift = match bitbase with
        | Oreg i ->
            let reg = op2e t bitbase in
            let shift = offset &* it (bits_of_width t - 1) t in
            reg, shift
        | Oaddr a ->
            let byte = load r8 (a +* (offset >>* (it 3 t))) in
            let shift = (cast_low r8 offset) &* (it 7 r8) in
            byte, shift
        | Oimm _ -> disfailwith "Immediate bases not allowed"
      in
      [
        move cf (cast_low r1 (value >>* shift));
	move oF (Unknown ("OF undefined after bt", r1));
	move sf (Unknown ("SF undefined after bt", r1));
	move af (Unknown ("AF undefined after bt", r1));
	move pf (Unknown ("PF undefined after bt", r1))
      ]
  | Bs(t, dst, src, dir) ->
    let source_is_zero = nt "t" r1 in
    let source_is_zero_v = Var source_is_zero in
    let src_e = op2e t src in
    let bits = bits_of_width t in
    let check_bit bitindex next_value =
      ite t (Extract(biconst bitindex,biconst bitindex,src_e) ==* it 1 r1) (it bitindex t) next_value
    in
    let bitlist = List.of_enum (0 --- (bits-1)) in
    (* We are folding from right to left *)
    let bitlist = match dir with
      | Forward -> (* least significant first *) bitlist
      | Backward -> (* most significant *) List.rev bitlist
    in
    let first_one = List.fold_right check_bit bitlist (Unknown("bs: destination undefined when source is zero", t)) in
    [
      move source_is_zero (src_e ==* it 0 t);
      assn t dst first_one;
      move zf (ite r1 source_is_zero_v (it 1 r1) (it 0 r1));
    ]
    @
      let undef (Var.V(_, n, t) as r) = move r (Unknown ((n^" undefined after bsf"), t)) in
      List.map undef [cf; oF; sf; af; pf]
  | Hlt ->
    [Halt(eax_e, [])]
  | Rdtsc ->
      [
        move eax (Unknown ("rdtsc", r32));
        move edx (Unknown ("rdtsc", r32));
      ]
  | Cpuid ->
      let undef reg = move reg (Unknown ("cpuid", r32)) in
      List.map undef [eax; ebx; ecx; edx]
  | Stmxcsr (dst) ->
      let dst = match dst with
        | Oaddr addr -> addr
        | _ -> disfailwith "stmxcsr argument cannot be non-memory"
      in
      [
        store r32 dst (Var mxcsr);(*(Unknown ("stmxcsr", r32));*)
      ]
  | Ldmxcsr (src) ->
      let src = match src with
        | Oaddr addr -> addr
        | _ -> disfailwith "ldmxcsr argument cannot be non-memory"
      in
      [
        move mxcsr (load r32 src);
      ]
  | Fnstcw (dst) ->
      let dst = match dst with
        | Oaddr addr -> addr
        | _ -> disfailwith "fnstcw argument cannot be non-memory"
      in
      [
        store r16 dst (Var fpu_ctrl);
      ]
  | Fldcw (src) ->
      let src = match src with
        | Oaddr addr -> addr
        | _ -> disfailwith "fldcw argument cannot be non-memory"
      in
      [
        move fpu_ctrl (load r16 src);
      ]
  | Fld (src) ->
    unimplemented "unsupported FPU register stack"
  | Fst (dst,pop) ->
    unimplemented "unsupported FPU flags"
  | Cmps(Reg bits as t) ->
    let src1 = nt "src1" t and src2 = nt "src2" t and tmpres = nt "tmp" t in
    let stmts =
      move src1 (op2e t esiaddr)
      :: move src2 (op2e_s seg_es t ediaddr)
      :: move tmpres (Var src1 -* Var src2)
      :: string_incr t esi
      :: string_incr t edi
      :: set_flags_sub t (Var src1) (Var src2) (Var tmpres)
    in
    if pref = [] then
      stmts
    else if pref = [repz] || pref = [repnz] then
      rep_wrap ~check_zf:(List.hd pref) ~addr ~next stmts
    else
      unimplemented "unsupported flags in cmps"
  | Scas(Reg bits as t) ->
    let src1 = nt "src1" t and src2 = nt "src2" t and tmpres = nt "tmp" t in
    let stmts =
      move src1 (cast_low t (Var eax))
      :: move src2 (op2e_s seg_es t ediaddr)
      :: move tmpres (Var src1 -* Var src2)
      :: string_incr t edi
      :: set_flags_sub t (Var src1) (Var src2) (Var tmpres)
    in
    if pref = [] then
      stmts
    else if pref = [repz] || pref = [repnz] then
      rep_wrap ~check_zf:(List.hd pref) ~addr ~next stmts
    else
      unimplemented "unsupported flags in scas"
  | Stos(Reg bits as t) ->
    let stmts = [store_s seg_es t edi_e (op2e t (o_eax));
		 string_incr t edi]
    in
    if pref = [] then
      stmts
    else if pref = [repz] then
      rep_wrap ~addr ~next stmts
    else
      unimplemented "unsupported prefix for stos"
  | Push(t, o) ->
    let tmp = nt "t" t in (* only really needed when o involves esp *)
    move tmp (op2e t o)
    :: move esp (esp_e -* i32 (bytes_of_width t))
    :: store_s seg_ss t esp_e (Var tmp) (* FIXME: can ss be overridden? *)
    :: []
  | Pop(t, o) ->
    (* From the manual:

       "The POP ESP instruction increments the stack pointer (ESP)
       before data at the old top of stack is written into the
       destination"

       So, effectively there is no incrementation.
    *)
    assn t o (load_s seg_ss t esp_e)
    :: if o = o_esp then []
      else [move esp (esp_e +* i32 (bytes_of_width t))]
  | Pushf(t) ->
    (* Note that we currently treat these fields as unknowns, but the
       manual says: When copying the entire EFLAGS register to the
       stack, the VM and RF flags (bits 16 and 17) are not copied;
       instead, the values for these flags are cleared in the EFLAGS
       image stored on the stack. *)
    let flags_e = match t with
      | Reg 16 -> flags_e
      | Reg 32 -> eflags_e
      | _ -> failwith "impossible"
    in
    move esp (esp_e -* i32 (bytes_of_width t))
    :: store_s seg_ss t esp_e flags_e
    :: []
  | Popf t ->
    let assnsf = match t with
      | Reg 16 -> assns_flags_to_bap
      | Reg 32 -> assns_eflags_to_bap
      | _ -> failwith "impossible"
    in
    let tmp = nt "t" t in
    let extractlist =
      List.map
        (fun i -> 
          let i = biconst i in
          extract i i (Var tmp))
        (BatList.of_enum (((bits_of_width t)-1)---0))
    in
    move tmp (load_s seg_ss t esp_e)
    :: move esp (esp_e +* i32 (bytes_of_width t))
    :: List.flatten (List.map2 (fun f e -> f e) assnsf extractlist)
  | Sahf ->
    let assnsf = assns_lflags_to_bap in
    let tah = nt "AH" reg_8 in
    let extractlist =
      List.map
        (fun i ->
          let i = biconst i in
          extract i i (Var tah))
        (BatList.of_enum (7---0))
    in
    move tah ah_e
    :: List.flatten (List.map2 (fun f e -> f e) assnsf extractlist)
  | Lahf ->
    let o_ah = Oreg 4 in
    [assn reg_8 o_ah lflags_e]
  | Add(t, o1, o2) ->
    let tmp = nt "t1" t and tmp2 = nt "t2" t in
    move tmp (op2e t o1)
    :: move tmp2 (op2e t o2)
    :: assn t o1 (op2e t o1 +* Var tmp2)
    :: let s1 = Var tmp and s2 = Var tmp2 and r = op2e t o1 in
       set_flags_add t s1 s2 r
  | Adc(t, o1, o2) ->
    let orig1 = nt "orig1" t and orig2 = nt "orig2" t in
    let bits = bits_of_width t in
    let t' = Reg (bits + 1) in
    let c = cast_unsigned t' in
    (* Literally compute the addition with an extra bit and see
       what the value is for CF *)
    let s1 = Var orig1 and s2 = Var orig2 and r = op2e t o1 in
    let bige = c s1 +* c s2 +* c (cast_unsigned t cf_e) in
    move orig1 (op2e t o1)
    :: move orig2 (op2e t o2)
    :: assn t o1 (s1 +* s2 +* cast_unsigned t cf_e)
    :: move cf (extract (biconst bits) (biconst bits) bige)
    :: set_aopszf_add t s1 s2 r
  | Inc(t, o) (* o = o + 1 *) ->
    let tmp = nt "t" t in
    move tmp (op2e t o)
    :: assn t o (op2e t o +* it 1 t)
    :: set_aopszf_add t (Var tmp) (it 1 t) (op2e t o)
  | Dec(t, o) (* o = o - 1 *) ->
    let tmp = nt "t" t in
    move tmp (op2e t o)
    :: assn t o (op2e t o -* it 1 t)
    :: set_aopszf_sub t (Var tmp) (it 1 t) (op2e t o) (* CF is maintained *)
  | Sub(t, o1, o2) (* o1 = o1 - o2 *) ->
    let oldo1 = nt "t" t in
    move oldo1 (op2e t o1)
    :: assn t o1 (op2e t o1 -* op2e t o2)
    :: set_flags_sub t (Var oldo1) (op2e t o2) (op2e t o1)
  | Sbb(t, o1, o2) ->
    let tmp_s = nt "ts" t in
    let tmp_d = nt "td" t in
    let orig_s = Var tmp_s in
    let orig_d = Var tmp_d in
    let sube = orig_s +* cast_unsigned t cf_e in
    let d = op2e t o1 in
    let s1 = op2e t o2 in
    move tmp_s s1
    :: move tmp_d d
    :: assn t o1 (orig_d -* sube)
    :: move oF (cast_high r1 ((orig_s ^* orig_d) &* (orig_d ^* d)))
    (* When src = 0xffffffff and cf=1, the processor sets CF=1.

       Note that we compute dest = dest - (0xffffffff + 1) = 0, so the
       subtraction does not overflow.

       So, I am guessing that CF is set if the subtraction overflows
       or the addition overflows.

       Maybe we should implement this by doing the actual computation,
       like we do for adc.
    *)
               (* sub overflow | add overflow *)
    :: move cf ((sube >* orig_d) |* (sube <* orig_s))
    :: move af (Unknown("AF for sbb unimplemented", r1))
    :: set_pszf t d
  | Cmp(t, o1, o2) ->
    let tmp = nt "t" t in
    move tmp (op2e t o1 -* op2e t o2)
    :: set_flags_sub t (op2e t o1) (op2e t o2) (Var tmp)
  | Cmpxchg(t, src, dst) ->
    let eax_e = op2e t o_eax in
    let dst_e = op2e t dst in
    let src_e = op2e t src in
    let tmp = nt "t" t in
    move tmp (eax_e -* dst_e)
    :: set_flags_sub t eax_e dst_e (Var tmp)
    @ assn t dst (ite t zf_e src_e dst_e)
    :: assn t o_eax (ite t zf_e eax_e dst_e)
    :: []
  | Cmpxchg8b o -> (* only 32bit case *)
    let accumulator = Concat((op2e r32 o_edx),(op2e r32 o_eax)) in
    let dst_e = op2e r64 o in
    let src_e = Concat((op2e r32 o_ecx),(op2e r32 o_ebx)) in
    let dst_low_e = Extract(biconst 63, biconst 32, dst_e) in
    let dst_hi_e = Extract(biconst 31, bi0, dst_e) in
    let eax_e = op2e r32 o_eax in
    let edx_e = op2e r32 o_edx in
    let equal = nt "t" r1 in
    let equal_v = Var equal in
    [
      move equal (accumulator ==* dst_e);
      move zf equal_v;
      assn r64 o (ite r64 equal_v src_e dst_e);
      assn r32 o_eax (ite r32 equal_v eax_e dst_low_e);
      assn r32 o_edx (ite r32 equal_v edx_e dst_hi_e)
    ]
  | Xadd(t, dst, src) ->
    let tmp = nt "t" t in
    move tmp (op2e t dst +* op2e t src)
    :: assn t src (op2e t dst)
    :: assn t dst (Var tmp)
    :: let s = Var tmp and src = op2e t src and dst = op2e t dst in
       set_flags_add t s src dst
  | Xchg(t, src, dst) ->
    let tmp = nt "t" t in
    [
      move tmp (op2e t src);
      assn t src (op2e t dst);
      assn t dst (Var tmp);
    ]
  | And(t, o1, o2) ->
    assn t o1 (op2e t o1 &* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after and", r1))
    :: set_pszf t (op2e t o1)
  | Or(t, o1, o2) ->
    assn t o1 (op2e t o1 |* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after or", r1))
    :: set_pszf t (op2e t o1)
  | Xor(t, o1, o2) when o1 = o2->
    assn t o1 (Int(bi0,t))
    :: move af (Unknown("AF is undefined after xor", r1))
    :: List.map (fun v -> move v exp_true) [zf; pf]
    @  List.map (fun v -> move v exp_false) [oF; cf; sf]
  | Xor(t, o1, o2) ->
    assn t o1 (op2e t o1 ^* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after xor", r1))
    :: set_pszf t (op2e t o1)
  | Test(t, o1, o2) ->
    let tmp = nt "t" t in
    move tmp (op2e t o1 &* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after and", r1))
    :: set_pszf t (Var tmp)
  | Ptest(t, o1, o2) ->
    let tmp1 = nt "t1" t in
    let tmp2 = nt "t2" t in
    move tmp1 (op2e t o2 &* op2e t o1)
    :: move tmp2 (op2e t o2 &* (exp_not (op2e t o1)))
    :: move af exp_false
    :: move oF exp_false
    :: move pf exp_false
    :: move sf exp_false
    :: move zf ((Var tmp1) ==* (Int(bi0, t)))
    :: [move cf ((Var tmp2) ==* (Int(bi0, t)))]
  | Not(t, o) ->
    [assn t o (exp_not (op2e t o))]
  | Neg(t, o) ->
    let tmp = nt "t" t in
    let min_int = 
      Ast_convenience.binop LSHIFT (it 1 t) (it ((bits_of_width t)-1) t)
    in
    move tmp (op2e t o)
    ::assn t o (it 0 t -* op2e t o)
    ::move cf (ite r1 (Var tmp ==* it 0 t) (it 0 r1) (it 1 r1))
    ::move oF (ite r1 (Var tmp ==* min_int) (it 1 r1) (it 0 r1))
    ::set_apszf_sub t (Var tmp) (it 0 t) (op2e t o)
  | Mul (t, src) ->
    (* Mul always multiplies EAX by src and stores the result in EDX:EAX 
       starting from the "right hand side" based on the type t of src *)

    (* The OF and CF flags are set to 0 if the upper half of the result is 0;
       otherwise, they are set to 1 *)
    let new_t = Reg ((bits_of_width t)*2) in
    let assnstmts, assne = assn_dbl t ((cast_unsigned new_t (op2e t o_eax)) ** (cast_unsigned new_t (op2e t src)))
    in
    let flag =
      let highbit = bits_of_width new_t - 1 in
      let lowbit = bits_of_width new_t / 2 in
      extract (biconst highbit) (biconst lowbit) assne <>* it 0 t
    in
    assnstmts
      @
      [
	move oF flag;
	move cf flag;
	move sf (Unknown("SF is undefined after Mul", r1));
	move zf (Unknown("ZF is undefined after Mul", r1));
	move af (Unknown("AF is undefined after Mul", r1));
	move pf (Unknown("PF is undefined after Mul", r1))
      ]
  | Imul (t, (oneopform, dst), src1, src2) -> 
    let new_t = Reg ((bits_of_width t)*2) in
    let mul_stmts = 
      (match oneopform with
      | true -> 
        (* For one operand form, use assn_double *)
        let assnstmts, assne = assn_dbl t ((cast_signed new_t (op2e t src1)) ** (cast_signed new_t (op2e t src2))) in
        let flag =
	  (* Intel checks if EAX == EDX:EAX.  Instead of doing this, we are just
	     going to check if the upper bits are != 0 *)
          let highbit = bits_of_width new_t - 1 in
          let lowbit = bits_of_width new_t / 2 in
          extract (biconst highbit) (biconst lowbit) assne <>* it 0 t
        in
        assnstmts @
	  [move oF flag;
	   move cf flag]
      | false ->
        (* Two and three operand forms *)
        let tmp = nt "t" new_t in
        (* Flag is set when the result is truncated *)
        let flag = (Var tmp <>* cast_signed new_t (op2e t dst)) in
        [(move tmp ((cast_signed new_t (op2e t src1)) ** (cast_signed new_t (op2e t src2))));
         (assn t dst (cast_low t (Var tmp)));
         move oF flag;
         move cf flag]
      )
    in
    mul_stmts@[
      move pf (Unknown("PF is undefined after imul", r1));
      move sf (Unknown("SF is undefined after imul", r1));
      move zf (Unknown("ZF is undefined after imul", r1));
      move af (Unknown("AF is undefined after imul", r1));
    ]
  | Div(t, src) ->
    let dt = Reg (bits_of_width t * 2) in
    let dividend = op2e_dbl t in
    let divisor = cast_unsigned dt (op2e t src) in
    let tdiv = nt "div" dt in
    let trem = nt "rem" dt in
    let assne = cast_low t (Var trem) ++* cast_low t (Var tdiv) in
    Assert(divisor <>* it 0 dt, [StrAttr "#DE"])
    :: move tdiv (dividend /* divisor)
    :: move trem (dividend %* divisor)
    (* Overflow is indicated with the #DE (divide error) exception
       rather than with the CF flag. *)
    :: [Assert(cast_high t (Var tdiv) ==* it 0 t, [StrAttr "#DE"])]
    @ fst (assn_dbl t assne)
    @ (let undef (Var.V(_, n, t) as r) = 
	 move r (Unknown ((n^" undefined after div"), t)) 
       in
       List.map undef [cf; oF; sf; zf; af; pf])
  | Idiv(t, src) ->
    let dt = Reg (bits_of_width t * 2) in
    let dividend = op2e_dbl t in
    let divisor = cast_signed dt (op2e t src) in
    let tdiv = nt "div" dt in
    let trem = nt "rem" dt in
    let assne = cast_low t (Var trem) ++* cast_low t (Var tdiv) in
    Assert(divisor <>* it 0 dt, [StrAttr "#DE"])
    :: move tdiv (dividend $/* divisor)
    :: [move trem (dividend $%* divisor)]
    (* Overflow is indicated with the #DE (divide error) exception
       rather than with the CF flag. *)
    (* SWXXX For signed division make sure quotient is between smallest and
       largest values.  For type t, this would be -2^(t/2) to (2^(t/2) - 1). *)
    (* :: [Assert(cast_high t (Var tdiv) ==* it 0 t, [StrAttr "#DE"])] *)
    @ fst (assn_dbl t assne)
    @ (let undef (Var.V(_, n, t) as r) = 
	 move r (Unknown ((n^" undefined after div"), t)) in
       List.map undef [cf; oF; sf; zf; af; pf])
  | Cld ->
    [Move(dflag, i32 1, [])]
  | Leave t when pref = [] -> (* #UD if Lock prefix is used *)
    Move(esp, ebp_e, [])
    ::to_ir addr next ss pref (Pop(t, o_ebp))
  | Interrupt(Oimm i) ->
    [Special(Printf.sprintf "int %Lx" i, [])]
  | Sysenter ->
    [Special("syscall", [])]
  (* Match everything exhaustively *)
  | Leave _ ->  unimplemented "to_ir: Leave"
  | Call _ ->  unimplemented "to_ir: Call"
  | Lea _ ->  unimplemented "to_ir: Lea"
  | Movsx _ ->  unimplemented "to_ir: Movsx"
  | Movzx _ ->  unimplemented "to_ir: Movzx"
  | Mov _ ->  unimplemented "to_ir: Mov"
  | Movs _ ->  unimplemented "to_ir: Movs"
  | Cmps _ ->  unimplemented "to_ir: Cmps"
  | Scas _ ->  unimplemented "to_ir: Scas"
  | Stos _ ->  unimplemented "to_ir: Stos"
  | Retn _ ->  unimplemented "to_ir: Retn"
  | Interrupt _ ->  unimplemented "to_ir: Interrupt"

let add_labels ?(asm) a ir =
  let attr = match asm with None -> [] | Some s -> [Asm(s)] in
  Label(Addr a, attr)
  ::Label(Name(Printf.sprintf "pc_0x%Lx" a),[])
  ::ir

end (* ToIR *)


module ToStr = struct

  let pref2str = function
(*  | Lock -> "lock"
  | Repnz -> "repnz"
  | Repz -> "repz"
  | Override _ | Hint_bnt | Hint_bt
  | Op_size | Mandatory_0f
  | Address_size -> failwith "finish pref2str" *)
    | _ -> unimplemented "pref2str"

  let rec prefs2str = function [] -> ""
    | x::xs -> pref2str x ^ " " ^ prefs2str xs

	  (* XXX Clean up printing here *)
  let oreg2str = function
	| 0 -> "eax"
	| 1 -> "ecx"
	| 2 -> "edx"
	| 3 -> "ebx"
	| 4 -> "exp"
	| 5 -> "ebp"
	| 6 -> "esi"
	| 7 -> "edi"
	| v -> unimplemented (Printf.sprintf "Don't know what oreg %i is." v)


  let opr = function
    | Oreg v -> oreg2str v
    | Oimm i -> Printf.sprintf "$0x%Lx" i
    | Oaddr a -> Pp.ast_exp_to_string a

  let j2str = function
    | Jabs o -> opr o
    | Jrel (_, offset) -> Printf.sprintf "+=%Ld" offset

  let op2str = function
    | Bswap(_, op) -> Printf.sprintf "bswap %s" (opr op)
    | Retn (op, _) -> 
      (match op with 
      | Some (_,src) -> Printf.sprintf "ret %s" (opr src)
      | None -> "ret")
    | Nop -> "nop"
    | Mov(t,d,s,None) -> Printf.sprintf "mov %s, %s" (opr d) (opr s)
    | Mov(t,d,s,Some(_)) -> Printf.sprintf "cmov %s, %s" (opr d) (opr s)
    | Movs(t) -> "movs"
    | Movzx(dt,dst,st,src) -> Printf.sprintf "movzx %s, %s" (opr dst) (opr src)
    | Movsx(dt,dst,st,src) -> Printf.sprintf "movsx %s, %s" (opr dst) (opr src)
    | Movdq(_t,td,d,ts,s,align,name) ->
      Printf.sprintf "%s %s, %s" name (opr d) (opr s)
    | Palignr(t,dst,src,imm) -> Printf.sprintf "palignr %s, %s, %s" (opr dst) (opr src) (opr imm)
    | Pcmpistri(t,dst,src,imm) -> Printf.sprintf "pcmpistri %s, %s, %s" (opr dst) (opr src) (opr imm)
    | Pshufd(dst,src,imm) -> Printf.sprintf "palignr %s, %s, %s" (opr dst) (opr src) (opr imm)
    | Pcmpeq(t,elet,dst,src) -> Printf.sprintf "pcmpeq %s, %s" (opr dst) (opr src)
    | Pmovmskb(t,dst,src) -> Printf.sprintf "pmovmskb %s, %s" (opr dst) (opr src)
    | Lea(r,a) -> Printf.sprintf "lea %s, %s" (opr r) (opr (Oaddr a))
    | Call(a, ra) -> Printf.sprintf "call %s" (opr a)
    | Shift _ -> "shift"
    | Shiftd _ -> "shiftd"
    | Rotate (rt, _, src, shift, use_cf) -> 
      let base = match rt with
	| LSHIFT -> if (use_cf) then "rcl" else "rol"
	| RSHIFT -> if (use_cf) then "rcr" else "ror"
	| _ -> disfailwith "imposible" in
      Printf.sprintf "%s %s, %s" base (opr src) (opr shift)
    | Hlt -> "hlt"
    | Rdtsc -> "rdtsc"
    | Cpuid -> "cpuid"
    | Stmxcsr (o) -> Printf.sprintf "stmxcr %s" (opr o)
    | Ldmxcsr (o) -> Printf.sprintf "ldmxcr %s" (opr o)
    | Fnstcw (o) -> Printf.sprintf "fnstcw %s" (opr o)
    | Fldcw (o) -> Printf.sprintf "fldcw %s" (opr o)
    | Fld (o) -> Printf.sprintf "fld %s" (opr o)
    | Fst (o,b) -> (match b with 
      | true -> Printf.sprintf "fstp %s" (opr o)
      | false -> Printf.sprintf "fst %s" (opr o))
    | Inc (t, o) -> Printf.sprintf "inc %s" (opr o)
    | Dec (t, o) -> Printf.sprintf "dec %s" (opr o)
    | Jump a -> Printf.sprintf "jmp %s" (j2str a)
    | Bt(t,d,s) -> Printf.sprintf "bt %s, %s" (opr d) (opr s)
    | Bs(t,d,s,dir) -> Printf.sprintf "bs%s %s, %s" (opr d) (opr s) (match dir with Forward -> "f" | Backward -> "r")
    | Jcc _ -> "jcc"
    | Setcc _ -> "setcc"
    | Cmps _ -> "cmps"
    | Scas _ -> "scas"
    | Stos _ -> "stos"
    | Push(t,o) -> Printf.sprintf "push %s" (opr o)
    | Pop(t,o) -> Printf.sprintf "pop %s" (opr o)
    | Pushf _ -> "pushf"
    | Popf _ -> "popf"
    | Sahf -> "sahf"
    | Lahf -> "lahf"
    | Add(t,d,s) -> Printf.sprintf "add %s, %s" (opr d) (opr s)
    | Adc(t,d,s) -> Printf.sprintf "adc %s, %s" (opr d) (opr s)
    | Sub(t,d,s) -> Printf.sprintf "sub %s, %s" (opr d) (opr s)
    | Sbb(t,d,s) -> Printf.sprintf "sbb %s, %s" (opr d) (opr s)
    | Cmp(t,d,s) -> Printf.sprintf "cmp %s, %s" (opr d) (opr s)
    | Cmpxchg(t,d,s) -> Printf.sprintf "cmpxchg %s, %s" (opr d) (opr s)
    | Cmpxchg8b(o) -> Printf.sprintf "cmpxchg8b %s" (opr o)
    | Xadd(t,d,s) -> Printf.sprintf "xadd %s, %s" (opr d) (opr s)
    | Xchg(t,d,s) -> Printf.sprintf "xchg %s, %s" (opr d) (opr s)
    | And(t,d,s) -> Printf.sprintf "and %s, %s" (opr d) (opr s)
    | Or(t,d,s) -> Printf.sprintf "or %s, %s" (opr d) (opr s)
    | Xor(t,d,s) -> Printf.sprintf "xor %s, %s" (opr d) (opr s)
    | Pxor(t,d,s)  -> Printf.sprintf "pxor %s, %s" (opr d) (opr s)
    | Test(t,d,s) -> Printf.sprintf "test %s, %s" (opr d) (opr s)
    | Ptest(t,d,s) -> Printf.sprintf "ptest %s, %s" (opr d) (opr s)
    | Not(t,o) -> Printf.sprintf "not %s" (opr o)
    | Neg(t,o) -> Printf.sprintf "neg %s" (opr o)
    | Mul (t, src) -> 
      Printf.sprintf "mul %s" (opr src)
    | Imul (t, (b,dst), src1, src2) -> 
      (match b with
      | true ->
	Printf.sprintf 
          "imul %s"  (opr src2)
      | false ->
	Printf.sprintf "imul %s, %s, %s" (opr dst) (opr src1) (opr src2))
    | Div(t, src) ->
      Printf.sprintf "div %s" (opr src)
    | Idiv(t, src) ->
      Printf.sprintf "idiv %s" (opr src)
    | Cld -> "cld"
    | Leave _ -> "leave"
    | Interrupt(o) -> Printf.sprintf "int %s" (opr o)
    | Sysenter -> "sysenter"

  let to_string pref op =
    disfailwith "fallback to libdisasm"
    (* prefs2str pref ^ op2str op *)
end (* ToStr *)

(* extract the condition to jump on from the opcode bits
for 70 to 7f and 0f 80 to 8f *)
let cc_to_exp i =
  let cc = match i & 0xe with
    | 0x0 -> of_e
    | 0x2 -> cf_e
    | 0x4 -> zf_e
    | 0x6 -> cf_e |* zf_e
    | 0x8 -> sf_e
    | 0xa -> pf_e
    | 0xc -> sf_e ^* of_e
    | 0xe -> zf_e |* (sf_e ^* of_e)
    | _ -> disfailwith "impossible condition code"
  in
  if (i & 1) = 0 then cc else exp_not cc

let parse_instr g addr =
  let s = Int64.succ in

  let get_prefix c =
    let i = Char.code c in
    match i with
    | 0xf0 | 0xf2 | 0xf3 | 0x2e | 0x36 | 0x3e | 0x26 | 0x64 | 0x65
    | 0x66 | 0x67 -> Some i
    | _ -> None
  in
  let get_prefixes a =
    let rec f l a =
      match get_prefix (g a) with
      | Some p -> f (p::l) (s a)
      | None -> (l, a)
    in
    f [] a
  in
(*  let int2prefix ?(jmp=false) = function
    | 0xf0 -> Some Lock
    | 0xf2 -> Some Repnz
    | 0xf3 -> Some Repz
    | 0x2e when jmp-> Some Hint_bnt
    | 0x3e when jmp-> Some Hint_bt
    | 0x2e -> Some(Override CS)
    | 0x36 -> Some(Override SS)
    | 0x3e -> Some(Override DS)
    | 0x26 -> Some(Override ES)
    | 0x64 -> Some(Override FS)
    | 0x65 -> Some(Override GS)
    | 0x66 -> Some Op_size
    | 0x0f -> Some Mandatory_0f
    | 0x67 -> Some Address_size
    | _ -> None
  in*)
  let parse_int8 a =
    (Int64.of_int (Char.code (g a)), s a)
  and parse_int16 a =
    let r n = Int64.shift_left (Int64.of_int (Char.code (g (Int64.add a (Int64.of_int n))))) (8*n) in
    let d = r 0 in
    let d = Int64.logor d (r 1) in
    (d, (Int64.add a 2L))
  and parse_int32 a =
    let r n = Int64.shift_left (Int64.of_int (Char.code (g (Int64.add a (Int64.of_int n))))) (8*n) in
    let d = r 0 in
    let d = Int64.logor d (r 1) in
    let d = Int64.logor d (r 2) in
    let d = Int64.logor d (r 3) in
    (d, (Int64.add a 4L))
  in
  let to_signed i t = 
    int64_of_big_int (Arithmetic.to_sbig_int (biconst64 i, t)) in
  let parse_sint8 a =
    let (i, na) = parse_int8 a in
    (to_signed i reg_8, na)
  and parse_sint16 a =
    let (i, na) = parse_int16 a in
    (to_signed i reg_16, na)
  and parse_sint32 a =
    let (i, na) = parse_int32 a in
    (to_signed i reg_32, na)
  in
  let parse_disp8 = parse_sint8
  and parse_disp16 = parse_sint16
  and parse_disp32 = parse_sint32
  in
  let parse_disp:(Type.typ -> int64 -> int64 * int64) = function
    | Reg 8 ->  parse_disp8
    | Reg 16 -> parse_disp16
    | Reg 32 -> parse_disp32
    | _ -> disfailwith "unsupported displacement size"
  in
  let parse_sib m a =
    (* ISR 2.1.5 Table 2-3 *)
    let b = Char.code (g a) in
    let ss = b >> 6 and idx = (b>>3) & 7 in
    let base, na = 
      match ((b & 7), m) with (* base register, MOD *)
      | 5, 0 -> let (i,na) = parse_disp32(s a) in (l32 i, na)
      | _, 0 | _, 1 | _, 2 -> (bits2reg32e (b & 7), s a)
      | _ -> disfailwith (Printf.sprintf "impossible opcode: sib b=%02x" b)
    in
    if idx = 4 then (base, na) else
      let idx = bits2reg32e idx in
      if ss = 0 then (base +* idx, na)
      else (base +* (idx <<* i32 ss), na)
  in
  let parse_modrm16ext a =
    (* ISR 2.1.5 Table 2-1 *)
    let b = Char.code (g a)
    and na = s a in
    let r = (b>>3) & 7
    and m = b >> 6
    and rm = b & 7 in
    match m with (* MOD *)
    | 0 -> (match rm with
      | 6 -> let (disp, na) = parse_disp16 (s a) in (r, Oaddr(l16 disp), na)
      | n when n < 8 -> (r, Oaddr(eaddr16 rm), s a)
      | _ -> disfailwith "Impossible"
    )
    | 1 | 2 ->
      let (base, na) = eaddr16 rm, na in
      let (disp, na) = 
	if m = 1 then parse_disp8 na else (*2*) parse_disp16 na in
      (r, Oaddr(base +* l16 disp), na)
    | 3 -> (r, Oreg rm, s a)
    | _ -> disfailwith "Impossible"
  in
  let parse_modrm16 a =
    let (r, rm, na) = parse_modrm16ext a in
    (Oreg r, rm, na)
  in
  let parse_modrm32ext a =
    (* ISR 2.1.5 Table 2-2 *)
    let b = Char.code (g a)
    and na = s a in
    let r = (b>>3) & 7
    and m = b >> 6
    and rm = b & 7 in
    match m with (* MOD *)
    | 0 -> (match rm with
      | 4 -> let (sib, na) = parse_sib m (s a) in (r, Oaddr sib, na)
      | 5 -> let (disp, na) = parse_disp32 (s a) in (r, Oaddr(l32 disp), na)
      | n -> (r, Oaddr(bits2reg32e n), s a)
    )
    | 1 | 2 ->
      let (base, na) = 
	if 4 = rm then parse_sib m na else (bits2reg32e rm, na) in
      let (disp, na) = 
	if m = 1 then parse_disp8 na else (*2*) parse_disp32 na in
      (r, Oaddr(base +* l32 disp), na)
    | 3 -> (r, Oreg rm, s a)
    | _ -> disfailwith "Impossible"
  in
  let parse_modrm32 a =
    let (r, rm, na) = parse_modrm32ext a in
    (Oreg r, rm, na)
(*  and parse_modrmxmm a =
    let (r, rm, na) = parse_modrm32ext a in
    let rm = match rm with Oreg r -> Oreg (reg2xmm r) | _ -> rm in
    (Oreg(bits2xmm r), rm, na) *)
  in
  let parse_modrm opsize a = parse_modrm32 a in
  (* Parse 8-bits as unsigned integer *)
  let parse_imm8 a = (* not sign extended *)
    let (i, na) = parse_int8 a in
    (Oimm i, na)
  and parse_simm8 a = (* sign extended *)
    let (i, na) = parse_sint8 a in
    (Oimm i, na)
  and parse_imm16 a =
    let (i, na) = parse_int16 a in
    (Oimm i, na)
  and parse_simm16 a =
    let (i, na) = parse_sint16 a in
    (Oimm i, na)
  and parse_imm32 a =
    let (i, na) = parse_int32 a in
    (Oimm i, na)
  and parse_simm32 a =
    let (i, na) = parse_sint32 a in
    (Oimm i, na)
  in
  let parse_immz t a = match t with
    | Reg 16 -> parse_imm16 a
    | Reg 32 | Reg 64 -> parse_imm32 a
    | _ -> disfailwith "parse_immz unsupported size"
  in
  let parse_immv = parse_immz in (* until we do amd64 *)
  let parse_immb = parse_imm8 in
  let parse_immw = parse_imm16 in 
  (* let parse_immd = parse_imm32 in *)
  let parse_simmb = parse_simm8 in
  let parse_simmw = parse_simm16 in
  let parse_simmd = parse_simm32 in
  (* sign extend op of type ot to size *)
  let sign_ext ot op size = (match op with
    | Oimm d ->
      let (v,_) = 
	Arithmetic.cast CAST_SIGNED ((biconst64 d), ot) size
      in
      (Oimm (int64_of_big_int v)) 
    | _ -> disfailwith "sign_ext only handles Oimm"
  ) in
  let get_opcode pref prefix a =
    (* We should rename these, since the 32 at the end is misleading. *)
    let parse_disp32, parse_modrm32, parse_modrm32ext =
      if prefix.addrsize_override 
      then parse_disp16, parse_modrm16, parse_modrm16ext
      else parse_disp32, parse_modrm32, parse_modrm32ext
    in
    let b1 = Char.code (g a)
    and na = s a in
    match b1 with (* Table A-2 *)
	(*** 00 to 3d are near the end ***)
    | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 ->
      (Inc(prefix.opsize, Oreg(b1 & 7)), na)
    | 0x48 | 0x49 | 0x4a | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f ->
      (Dec(prefix.opsize, Oreg(b1 & 7)), na)
    | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 ->
      (Push(prefix.opsize, Oreg(b1 & 7)), na)
    | 0x58 | 0x59 | 0x5a | 0x5b | 0x5c | 0x5d | 0x5e | 0x5f ->
      (Pop(prefix.opsize, Oreg(b1 & 7)), na)
    | 0x68 | 0x6a  ->
      let (o, na) = 
	(* SWXXX Sign extend these? *)
	if b1=0x68 then parse_immz prefix.opsize na else parse_immb na 
      in
      (Push(prefix.opsize, o), na)
    | 0x69 | 0x6b ->
      let (r, rm, na) = parse_modrm prefix.opsize na in
      let ((o, na), ot) = 
	if b1 = 0x6b then (parse_simmb na, r8) else 
	  if (prefix.opsize = r16) then (parse_simmw na, r16) 
	  else (parse_simmd na, r32)
      in
      (Imul(prefix.opsize, (false,r), rm, (sign_ext ot o prefix.opsize)), na)
    | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78 | 0x79
    | 0x7a | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f -> 
      let (i,na) = parse_disp8 na in
      (Jcc(Jabs(Oimm(Int64.add i na)), cc_to_exp b1), na)
    | 0x80 | 0x81 | 0x82 | 0x83 -> 
      let (r, rm, na) = parse_modrm32ext na in
      let (o2, na) =
	if b1 = 0x81 then parse_immz prefix.opsize na
	else let (o,na) = (parse_simmb na)
	     in ((sign_ext r8 o prefix.opsize), na)
      in
      let opsize = if b1 land 1 = 0 then r8 else prefix.opsize in
      (match r with (* Grp 1 *)
      | 0 -> (Add(opsize, rm, o2), na)
      | 1 -> (Or(opsize, rm, o2), na)
      | 2 -> (Adc(opsize, rm, o2), na)
      | 3 -> (Sbb(opsize, rm, o2), na)
      | 4 -> (And(opsize, rm, o2), na)
      | 5 -> (Sub(opsize, rm, o2), na)
      | 6 -> (Xor(opsize, rm, o2), na)
      | 7 -> (Cmp(opsize, rm, o2), na)
      | _ -> disfailwith  
	(Printf.sprintf "impossible opcode: %02x/%d" b1 r)
      )
    | 0x84
    | 0x85 -> let (r, rm, na) = parse_modrm32 na in
	      let o = if b1 = 0x84 then r8 else prefix.opsize in
	      (Test(o, rm, r), na)
    | 0x87 -> let (r, rm, na) = parse_modrm prefix.opsize na in
	      (Xchg(prefix.opsize, r, rm), na)
    | 0x88 -> let (r, rm, na) = parse_modrm r8 na in
	      (Mov(r8, rm, r, None), na)
    | 0x89 -> let (r, rm, na) = parse_modrm32 na in
	      (Mov(prefix.opsize, rm, r, None), na)
    | 0x8a -> let (r, rm, na) = parse_modrm r8 na in
	      (Mov(r8, r, rm, None), na)
    | 0x8b -> let (r, rm, na) = parse_modrm32 na in
	      (Mov(prefix.opsize, r, rm, None), na)
    | 0x8d -> let (r, rm, na) = parse_modrm prefix.opsize na in
	      (match rm with
	      | Oaddr a -> (Lea(r, a), na)
	      | _ -> disfailwith "invalid lea (must be address)"
	      )
    | 0x90 -> (Nop, na)
    | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 ->
      let reg = Oreg (b1 & 7) in
      (Xchg(prefix.opsize, o_eax, reg), na)
    | 0x9c -> (Pushf(prefix.opsize), na)
    | 0x9d -> (Popf(prefix.opsize), na)
    | 0x9e -> (Sahf, na)
    | 0x9f -> (Lahf, na)
    | 0xa1 -> let (addr, na) = parse_disp32 na in
	      (Mov(prefix.opsize, o_eax, Oaddr(l32 addr), None), na)
    | 0xa3 -> let (addr, na) = parse_disp32 na in
	      (Mov(prefix.opsize, Oaddr(l32 addr), o_eax, None), na)
    | 0xa4 -> (Movs r8, na)
    | 0xa5 -> (Movs prefix.opsize, na)
    | 0xa6 -> (Cmps r8, na)
    | 0xa7 -> (Cmps prefix.opsize, na)
    | 0xae -> (Scas r8, na)
    | 0xaf -> (Scas prefix.opsize, na)
    | 0xa8 -> let (i, na) = parse_imm8 na in
	      (Test(r8, o_eax, i), na)
    | 0xa9 -> let (i,na) = parse_immz prefix.opsize na in
	      (Test(prefix.opsize, o_eax, i), na)
    | 0xaa -> (Stos r8, na)
    | 0xab -> (Stos prefix.opsize, na)
    | 0xb0 | 0xb1 | 0xb2 | 0xb3 | 0xb4 | 0xb5 | 0xb6
    | 0xb7 -> let (i, na) = parse_imm8 na in
	      (Mov(r8, Oreg(b1 & 7), i, None), na)
    | 0xb8 | 0xb9 | 0xba | 0xbb | 0xbc | 0xbd | 0xbe
    | 0xbf -> let (i, na) = parse_immv prefix.opsize na in
	      (Mov(prefix.opsize, Oreg(b1 & 7), i, None), na)
    | 0xc2 | 0xc3 (* Near ret *)
    | 0xca | 0xcb (* Far ret *)-> 
      let far_ret = if (b1 = 0xc2 or b1 = 0xc3) then false else true in
      if (b1 = 0xc3 or b1 = 0xcb) then (Retn(None, far_ret), na) 
      else let (imm,na) = parse_immw na in 
	   (Retn(Some(r32, imm), far_ret), na)
    | 0xc6
    | 0xc7 -> let t = if b1 = 0xc6 then r8 else prefix.opsize in
	      let (e, rm, na) = parse_modrm32ext na in
	      let (i,na) = 
		if b1 = 0xc6 then parse_immb na else parse_immz t na 
	      in
	      (match e with (* Grp 11 *)
	      | 0 -> (Mov(t, rm, i, None), na)
	      | _ -> disfailwith (Printf.sprintf "Invalid opcode: %02x/%d" b1 e)
	      )
    | 0xc9 -> (Leave prefix.opsize, na)
    | 0xcd -> let (i,na) = parse_imm8 na in
	      (Interrupt(i), na)
                
    (* 0xd8-0xdf can be followed by a secondary opcode, OR a modrm
       byte. But the secondary opcode is only used when the modrm
       byte does not specify a memory address. *)
    | 0xd8 | 0xd9 | 0xda | 0xdb | 0xdc | 0xdd | 0xde | 0xdf ->
      let b2, _ = parse_int8 na in
      let (r, rm, na) = parse_modrm32ext na in
      (match r, rm with
      | 2, Oaddr _ -> 
        (match b1 with 
        | 0xd9 | 0xdd -> (Fst(rm, false), na)
        | _ -> 
          unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
        )
      | 3, Oaddr _ -> 
        (match b1 with 
        | 0xd9 | 0xdd -> (Fst(rm, true), na)
        | _ -> 
          unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
        )
      | 5, Oaddr _ -> 
        (match b1 with 
        | 0xd9 -> (Fldcw rm, na)
        | 0xdb -> (Fld rm, na)
        | _ -> 
          unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
        )
      | 7, Oaddr _ -> 
        (match b1 with 
        | 0xd9 -> (Fnstcw rm, na)
        | 0xdb -> (Fst(rm, true), na)
        | _ -> 
          unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
        )
      | _, Oaddr _ -> 
        unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
      | _, _ -> 
        unimplemented (Printf.sprintf "unsupported opcode: %02x %02Lx" b1 b2)
      )

    | 0xe8 -> let (i,na) = parse_disp32 na in
	      (Call(Oimm(Int64.add i na), na), na)
    | 0xe9 -> let (i,na) = parse_disp prefix.opsize na in
	      (Jump(Jabs(Oimm(Int64.add i na))), na)
    | 0xeb -> let (i,na) = parse_disp8 na in
	      (Jump(Jabs(Oimm(Int64.add i na))), na)
    | 0xc0 | 0xc1
    | 0xd0 | 0xd1 | 0xd2
    | 0xd3 -> let (r, rm, na) = parse_modrm32ext na in
	      let opsize = if (b1 & 1) = 0 then r8 else prefix.opsize in
	      let (amt, na) = match b1 & 0xfe with
		| 0xc0 -> parse_imm8 na
		| 0xd0 -> (Oimm 1L, na)
		| 0xd2 -> (o_ecx, na)
		| _ -> 
		  disfailwith (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
	      in
	      (match r with (* Grp 2 *)
	      | 0 -> (Rotate(LSHIFT, opsize, rm, amt, false),na)
	      | 1 -> (Rotate(RSHIFT, opsize, rm, amt, false),na)
		(* SWXXX Implement these *)
	      | 2 -> unimplemented 
		(* (Rotate(LSHIFT, opsize, rm, amt, true),na) *)
		(Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
	      | 3 -> unimplemented 
		(* (Rotate(RSHIFT, opsize, rm, amt, true),na) *)
		(Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
	      | 4 -> (Shift(LSHIFT, opsize, rm, amt), na)
	      | 5 -> (Shift(RSHIFT, opsize, rm, amt), na)
	      | 7 -> (Shift(ARSHIFT, opsize, rm, amt), na)
	      | _ -> disfailwith 
		(Printf.sprintf "impossible opcode: %02x/%d" b1 r)
	      )
    | 0xe3 ->
      let (i,na) = parse_disp8 na in
      (Jcc(Jrel(na, i), ecx_e ==* l32 0L), na)
    | 0xf4 -> (Hlt, na)
    | 0xf6
    | 0xf7 -> let t = if b1 = 0xf6 then r8 else prefix.opsize in
	      let (r, rm, na) = parse_modrm32ext na in
	      (match r with (* Grp 3 *)
	       | 0 ->
		 let (imm, na) = 
		   if (b1 = 0xf7) then parse_immz t na else parse_immb na
		 in 
		 (Test(t, rm, imm), na)
	       | 2 -> (Not(t, rm), na)
	       | 3 -> (Neg(t, rm), na)
	       | 4 -> 
		 (match b1 with 
		 | 0xf6 -> (Mul(t, rm), na)
		 | 0xf7 -> (Mul(t, rm), na)
		 | _ -> disfailwith
		   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
		 ) 
	       | 5 -> 
		 (match b1 with 
                 | 0xf6 -> (Imul(t, (true,o_eax), o_eax, rm), na)
                 | 0xf7 -> (Imul(t, (true,o_edx), o_eax, rm), na)
		 | _ -> disfailwith
		   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
		 )
	       | 6 -> 
		 (match b1 with
                 | 0xf6 -> (Div(reg_8, rm) , na)
                 | 0xf7 -> (Div(t, rm), na)
		 | _ -> disfailwith
		   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
		 )
	       | 7 -> 
		 (match b1 with
                 | 0xf6 -> (Idiv(reg_8, rm) , na)
                 | 0xf7 -> (Idiv(t, rm), na)
		 | _ -> disfailwith
		   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
		 )
	       | _ -> 
		 disfailwith (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
	      )
    | 0xfc -> (Cld, na)
    | 0xfe -> let (r, rm, na) = parse_modrm32ext na in
	      (match r with (* Grp 4 *)
                | 0 -> (Inc(r8, rm), na)
                | 1 -> (Dec(r8, rm), na)
	        | _ -> disfailwith 
		  (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
	      )
    | 0xff -> let (r, rm, na) = parse_modrm32ext na in
	      (match r with (* Grp 5 *)
                | 0 -> (Inc(prefix.opsize, rm), na)
                | 1 -> (Dec(prefix.opsize, rm), na)
	        | 2 -> (Call(rm, na), na)
		| 3 -> unimplemented (* callf *)
		  (Printf.sprintf "unsupported opcode: %02x/%d" b1 r) 
	        | 4 -> (Jump (Jabs rm), na)
		| 5 -> unimplemented (* jmpf *)
		  (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
	        | 6 -> (Push(prefix.opsize, rm), na)
	        | _ -> disfailwith 
		  (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
	      )
    (*** 00 to 3e ***)
    | b1 when b1 < 0x3e && (b1 & 7) < 6 ->
      (
	let ins a = match b1 >> 3 with
	  | 0 -> Add a
	  | 1 -> Or a
	  | 2 -> Adc a
	  | 3 -> Sbb a
	  | 4 -> And a
	  | 5 -> Sub a
	  | 6 -> Xor a
	  | 7 -> Cmp a
	  | _ -> disfailwith (Printf.sprintf "impossible opcode: %02x" b1)
	in
	let t = if (b1 & 1) = 0  then r8 else prefix.opsize in
	let (o1, o2, na) = match b1 & 7 with
	  | 0 | 1 -> let r, rm, na = parse_modrm t na in
		 (rm, r, na)
	  | 2 | 3 -> let r, rm, na = parse_modrm t na in
		 (r, rm, na)
	  | 4 -> let i, na = parse_immb na in
		 (o_eax, i, na)
	  | 5 ->let i, na = parse_immz t na in
		  (o_eax, i, na)
	  | _ -> disfailwith (Printf.sprintf "impossible opcode: %02x" b1)
	in
	(ins(t, o1, o2), na)
      )
    (* Two byte opcodes *)
    | 0x0f -> (
      let b2 = Char.code (g na) and na = s na in
      match b2 with (* Table A-3 *)
      | 0x1f -> (Nop, na)
      | 0x12 | 0x13 | 0x28 | 0x29 | 0x6e | 0x7e | 0x6f | 0x7f | 0xd6 ->
        (* XXX: Clean up prefixes.  This will probably require some
           effort studying the manual. We probably don't do the right
           thing on weird cases (too many prefixes set). *)
        let t, name, align, tsrc, tdest = match b2 with
          | 0x12 | 0x13 when prefix.opsize_override ->
            r128, "movlpd", false, r64, r64
          | 0x12 | 0x13 when not prefix.opsize_override ->
            r128, "movlps", false, r64, r64
          | 0x28 | 0x29 when prefix.opsize_override ->
	    r128, "movapd", true, r128, r128
          | 0x28 | 0x29 when not prefix.opsize_override -> 
	    r128, "movaps", true, r128, r128
          | 0x6f | 0x7f when prefix.repeat -> r128, "movdqu", false, r128, r128
          | 0x6f | 0x7f when prefix.opsize_override -> 
	    r128, "movdqa", true, r128, r128
          | 0x6e -> r32, "movd", false, r32, prefix.mopsize
          | 0x7e -> r32, "movd", false, prefix.mopsize, r32
          | 0x6f | 0x7f when pref=[] -> r64, "movq", false, r64, r64
          | 0xd6 when prefix.opsize_override -> r64, "movq", false, r64, r64
          | _ -> unimplemented
	    (Printf.sprintf "mov opcode case missing: %02x" b2)
        in
	let r, rm, na = parse_modrm32 na in
	let s, d = match b2 with
          | 0x12 | 0x6f | 0x6e | 0x28 -> rm, r
          | 0x13 | 0x7f | 0x7e | 0x29 | 0xd6 -> r, rm
	  | _ -> disfailwith 
	    (Printf.sprintf "impossible mov(a/d) condition: %02x" b2)
        in
	(Movdq(t, tdest, d, tsrc, s, align, name), na)
      | 0x31 -> (Rdtsc, na)
      | 0x34 -> (Sysenter, na)
      | 0x38 when prefix.opsize_override ->
        let b2 = Char.code (g na) and na = s na in
        (match b2 with
        | 0x17 ->
          let d, s, na = parse_modrm32 na in
          (Ptest(r128, d, s), na)
        | _ -> disfailwith (Printf.sprintf "opcode missing: %02x" b2))
      | 0x3a ->
        let b3 = Char.code (g na) and na = s na in
        (match b3 with
        | 0x0f ->
          let (r, rm, na) = parse_modrm prefix.opsize na in
	  let (i, na) = parse_imm8 na in
          (Palignr(prefix.mopsize, r, rm, i), na)
        | 0x63 ->
          let (r, rm, na) = parse_modrm prefix.opsize na in
          let (i, na) = parse_imm8 na in
          (match i with
                 (* See Section 4.1 of Intel manual for more
                    information on the immediate control byte.

                    i[0]:
                    0 = 16 packed bytes
                    1 =  8 packed words
                    i[1]:
                    0 = packed elements are unsigned
                    1 = packed elements are signed
                    i[3:2]:
                    00 = "equal any"
                    01 = "ranges"
                    10 = "each each"
                    11 = "equal ordered"
                    i[4]:
                    0 = IntRes1 unmodified
                    1 = IntRes1 is negated (1's complement)
                    i[5]:
                    0 = Negation of IntRes1 is for all 16 (8) bits
                    1 = Negation of IntRes1 is masked by reg/mem validity
                    i[6]:
                    0 = Use least significant bit for IntRes2
                    1 = Use most significant bit for IntRes2
                    i[7]: Undefined, set to 0.
                 *)

                 (* Note: We only implement the case for unsigned
                    bytes equal ordered comparison for pcmpistri right
                    now.

                    XXX: When we implement the other cases, we should
                    extract the control byte information into a record
                    type.
                 *)
                 | Oimm 0xcL -> (Pcmpistri(prefix.mopsize, r, rm, i), na)
                 | Oimm op  ->  unimplemented 
		   (Printf.sprintf "unsupported pcmpistri op %02Lx" op)
                 | _ ->  unimplemented "unsupported non-imm op for pcmpisgtri")
             | b4 ->  unimplemented  
	       (Printf.sprintf "unsupported opcode %02x %02x %02x" b1 b2 b3)
          )
      (* conditional moves *)
      | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 
      | 0x4a | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f ->
	let (r, rm, na) = parse_modrm32 na in
	(Mov(prefix.opsize, r, rm, Some(cc_to_exp b2)), na)
      | 0x70 when prefix.opsize = r16 ->
          let r, rm, na = parse_modrm prefix.opsize na in
          let i, na = parse_imm8 na in
          (Pshufd(r, rm, i), na)
      | 0x74 | 0x75 | 0x76 as o ->
        let r, rm, na = parse_modrm32 na in
        let elet = match o with | 0x74 -> r8 | 0x75 -> r16 | 0x76 -> r32 | _ ->
	  disfailwith "impossible" in
        (Pcmpeq(prefix.mopsize, elet, r, rm), na)
      | 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 | 0x89
      | 0x8a | 0x8b | 0x8c | 0x8d | 0x8e | 0x8f ->
        let (i,na) = parse_disp32 na in
	(Jcc(Jabs(Oimm(Int64.add i na)), cc_to_exp b2), na)
    (* add other opcodes for setcc here *)
      | 0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 | 0x99
      | 0x9a | 0x9b | 0x9c | 0x9d | 0x9e | 0x9f ->
        let r, rm, na = parse_modrm r8 na in
	(* unclear what happens otherwise *)
	assert (prefix.opsize = r32);
	(Setcc(r8, rm, cc_to_exp b2), na)
      | 0xa2 -> (Cpuid, na)
      | 0xa3 | 0xba ->
          let (r, rm, na) = parse_modrm prefix.opsize na in
          let r, na = if b2 = 0xba then parse_imm8 na else r, na in
          (Bt(prefix.opsize, r, rm), na)
      | 0xa4 ->
	(* shld *)
        let (r, rm, na) = parse_modrm prefix.opsize na in
	let (i, na) = parse_imm8 na in
	(Shiftd(LSHIFT, prefix.opsize, rm, r, i), na)
      | 0xa5 ->
	(* shld *)
        let (r, rm, na) = parse_modrm prefix.opsize na in
	(Shiftd(LSHIFT, prefix.opsize, rm, r, o_ecx), na)
      | 0xac ->
	(* shrd *)
        let (r, rm, na) = parse_modrm prefix.opsize na in
	let (i, na) = parse_imm8 na in
	(Shiftd(RSHIFT, prefix.opsize, rm, r, i), na)
      | 0xad ->
	(* shrd *)
        let (r, rm, na) = parse_modrm prefix.opsize na in
	(Shiftd(RSHIFT, prefix.opsize, rm, r, o_ecx), na)
      | 0xae ->
          let (r, rm, na) = parse_modrm32ext na in
          (match r with
             | 2 -> (Ldmxcsr rm, na) (* ldmxcsr *)
             | 3 -> (Stmxcsr rm, na) (* stmxcsr *)
             | _ -> unimplemented 
	       (Printf.sprintf "unsupported opcode: %02x %02x/%d" b1 b2 r)
          )
      | 0xaf ->
	let (r, rm, na) = parse_modrm prefix.opsize na in
	(Imul(prefix.opsize, (false,r), r, rm), na)
      | 0xb1 ->
        let r, rm, na = parse_modrm prefix.opsize na in
        (Cmpxchg (prefix.opsize, r, rm), na)
      | 0xb6
      | 0xb7 -> let st = if b2 = 0xb6 then r8 else r16 in
		let r, rm, na = parse_modrm32 na in
		(Movzx(prefix.opsize, r, st, rm), na)
      | 0xbc | 0xbd ->
        let dir = match b2 with | 0xbc -> Forward | 0xbd -> Backward | _ -> failwith "impossible" in
        let r, rm, na = parse_modrm prefix.opsize na in
        (Bs (prefix.opsize, r, rm, dir), na)
      | 0xbe
      | 0xbf -> let st = if b2 = 0xbe then r8 else r16 in
          let r, rm, na = parse_modrm32 na in
          (Movsx(prefix.opsize, r, st, rm), na)
      | 0xc1 ->
          let r, rm, na = parse_modrm32 na in
          (Xadd(prefix.opsize, r, rm), na)
      | 0xc7 ->
          let r, rm, na = parse_modrm32ext na in
          (match r with
            | 1 -> (Cmpxchg8b(rm), na)
            | _ -> unimplemented 
	      (Printf.sprintf "unsupported opcode: %02x %02x/%d" b1 b2 r)
          )
      | 0xc8 | 0xc9 | 0xca | 0xcb | 0xcc | 0xcd | 0xce | 0xcf ->
        (Bswap(prefix.opsize, Oreg(b2 & 7)), na)
      | 0xd7 ->
          let r, rm, na = parse_modrm32 na in
          (Pmovmskb(prefix.mopsize, r, rm), na)
      | 0xef ->
	let d, s, na = parse_modrm32 na in
	(Pxor(prefix.mopsize,d,s), na)
      | _ -> unimplemented 
	(Printf.sprintf "unsupported opcode: %02x %02x" b1 b2)
    )
    | n -> unimplemented (Printf.sprintf "unsupported opcode: %02x" n)

  in
  let pref, a = get_prefixes addr in
  (* Opsize for regular instructions, MMX/SSE2 instructions

     The opsize override makes regular operands smaller, but MMX
     operands larger.  *)
  let opsize, mopsize = 
    if List.mem pref_opsize pref then r16,r128 else r32,r64 in
  let prefix =
    {
      opsize = opsize;
      mopsize = mopsize;
      repeat = List.mem repz pref;
      nrepeat = List.mem repnz pref;
      addrsize_override = List.mem pref_addrsize pref;
      opsize_override = List.mem pref_opsize pref
    }
  in
  let op, a = get_opcode pref prefix a in
  (pref, op, a)

let parse_prefixes pref op =
  (* FIXME: how to deal with conflicting prefixes? *)
  let rec f t s r = function
    | [] -> (t, s, List.rev r)
    | 0x2e::p -> f t seg_cs r p
    | 0x36::p -> f t seg_ss r p
    | 0x3e::p -> f t seg_ds r p
    | 0x26::p -> f t seg_es r p
    | 0x64::p -> f t seg_fs r p
    | 0x65::p -> f t seg_gs r p
    | 0xf0::p -> f t s r p (* discard lock prefix *)
    | 0x66::p -> f r16 s r p
    | p::ps -> f t s (p::r) ps
  in
  f r32 None [] pref

let disasm_instr g addr =
  let (pref, op, na) = parse_instr g addr in
  let (_, ss, pref) =  parse_prefixes pref op in
  let ir = ToIR.to_ir addr na ss pref op in
  let asm = 
    try Some(ToStr.to_string pref op) with Disasm_i386_exception _ -> None 
  in
  (ToIR.add_labels ?asm addr ir, na)
