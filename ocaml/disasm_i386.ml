open Int64
open Ast
open Type

module D = Debug.Make(struct let name = "Disasm_i386" and default=`Debug end)
open D

(* To help understand this file, please refer to the
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
*)

type segment = CS | SS | DS | ES | FS | GS
type prefix =
  | Lock | Repnz | Repz
  | Override of segment | Hint_bnt | Hint_bt
  | Op_size | Mandatory_0f
  | Address_size

type operand =
  | Oreg of Var.t
  | Oaddr of Ast.exp
  | Oimm of int64

type opcode =
  | Retn
  | Mov of operand * operand (* dst, src *)
  | Call of operand * int64 (* Oimm is relative, int64 is RA *)
  | Shift of binop_type * typ * operand * operand

let unimplemented s  = failwith ("disasm_i386: unimplemented feature: "^s)

let (&) = (land)
and (>>) = (lsr)
and (<<) = (lsl)


(* register widths *)
let r1 = Ast.reg_1
let r8 = Ast.reg_8
let r16 = Ast.reg_16
let r32 = Ast.reg_32

let nv = Var.newvar
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
and eflags = nv "EFLAGS" r32
  (* condition flag bits *)
and cf = nv "R_CF" r1
and pf = nv "R_PF" r1
and af = nv "R_AF" r1
and zf = nv "R_ZF" r1
and sf = nv "R_SF" r1
and oF = nv "R_OF" r1


let regs : var list =
  ebp::esp::esi::edi::eip::eax::ebx::ecx::edx::eflags::cf::pf::af::zf::sf::oF::
  List.map (fun (n,t) -> Var.newvar n t)
    [

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



let esp_e = Var esp

let mem = nv "mem" (TMem(r32))
let mem_e = Var mem
and cf_e = Var cf
and pf_e = Var pf
and af_e = Var af
and zf_e = Var zf
and sf_e = Var sf
and of_e = Var oF


(* exp helpers *)
let load t a =
  Load(mem_e, a, little_endian, t)


let (+*) a b = BinOp(PLUS, a, b)
let (-*) a b = BinOp(MINUS, a, b)
let (<<*) a b = BinOp(LSHIFT, a, b)
let (>>*) a b = BinOp(RSHIFT, a, b)
let (>>>*) a b = BinOp(ARSHIFT, a, b)
let (&*) a b = BinOp(AND, a, b)
let (|*) a b = BinOp(OR, a, b)
let (^*) a b = BinOp(XOR, a, b)
let (=*) a b = BinOp(EQ, a, b)

let ite t b e1 e2 = (* FIXME: were we going to add a native if-then-else thing? *)
  (Cast(CAST_SIGNED, t, b) &* e1) |*  (Cast(CAST_SIGNED, t, exp_not b) &* e2) 

let l32 i = Int(i, r32)
let i32 i = Int(Int64.of_int i, r32)

module ToIR = struct

(* stmt helpers *)
let move v e =
  Move(v, e, [])

let store t a e =
  move mem (Store(mem_e, a, e, little_endian, t))

let op2e t = function
  | Oreg r -> Var r
  | Oaddr e -> load t e
  | Oimm i -> Int(i, t)

let assn t v e =
  match v with
  | Oreg r -> move r e
  | Oaddr a -> store t a e
  | Oimm _ -> failwith "disasm_i386: Can't assign to an immediate value"


let reta = [StrAttr "ret"]
and calla = [StrAttr "call"]

let compute_sf result = Cast(CAST_HIGH, r1, result)
let compute_zf t result = Int(0L, t) =* result
let compute_pf result = Cast(CAST_LOW, r1, result)

let to_ir pref = function
  | Retn when pref = [] ->
    let t = nv "ra" r32 in
    [move t (load r32 esp_e);
     move esp (esp_e +* (i32 4));
     Jmp(Var t, [StrAttr "ret"])
    ]
  | Mov(dst,src) when pref = [] ->
    [assn r32 dst (op2e r32 src)] (* FIXME: r32 *)
  | Call(Oimm i, ra) when pref = [] ->
    [move esp (esp_e -* i32 4);
     store r32 esp_e (l32 ra);
     Jmp(l32(Int64.add i ra), calla)]
  | Shift(st, s, o1, o2) when pref = [] || pref = [Op_size] ->
    assert (List.mem s [r8; r16; r32]);
    (* FIXME: how should we deal with undefined state? *)
    let t1 = nv "t1" s and tmpDEST = nv "tmpDEST" s
    and bits = Arithmetic.bits_of_width s
    and s_f = match st with LSHIFT -> (<<*) | RSHIFT -> (>>*) | ARSHIFT -> (>>>*)
      | _ -> failwith "invalid shift type"
    and count = (op2e r32 o2) &* i32 31
    and e1 = op2e s o1 in
    let ifzero = ite r1 (count =* i32 0)
    and our_of = match st with
      | LSHIFT -> Cast(CAST_HIGH, r1, e1) ^* cf_e
      | RSHIFT -> Cast(CAST_HIGH, r1, Var tmpDEST)
      | ARSHIFT -> exp_false
      | _ -> failwith "imposible"
    in
    [move tmpDEST e1;
     if st = LSHIFT then
       move t1 (e1 >>* (i32 bits -* count))
      else
       move t1 (e1 <<* (count -* i32 1));
     move cf (ifzero cf_e (Cast(CAST_LOW, r1, Var t1)));
     assn s o1 (s_f e1 count);
     move oF (ifzero of_e (ite r1 (count =* i32 1) (our_of) (Unknown("OF <- undefined", r1))));
     move sf (ifzero sf_e (compute_sf e1));
     move zf (ifzero zf_e (compute_zf s e1));
     move pf (ifzero pf_e (compute_pf e1))
    ]
  | _ -> unimplemented "to_ir"

let add_labels a ir =
  Label(Addr a,[])
  ::Label(Name(Printf.sprintf "pc_0x%Lx" a),[])
  ::ir

end (* ToIR *)


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

let bits2reg32e b = Var(bits2reg32 b)

let disasm_instr g addr =
  let s = Int64.succ in

  let get_prefix = function
    | '\xf0' -> Some Lock
    | '\xf2' -> Some Repnz
    | '\xf3' -> Some Repz
    | '\x2e' -> Some(Override CS)
    | '\x36' -> Some(Override SS)
    | '\x3e' -> Some(Override DS)
    | '\x26' -> Some(Override ES)
    | '\x64' -> Some(Override FS)
    | '\x65' -> Some(Override GS)
(*    | '\x2e' -> Some Hint_bnt
    | '\x3e' -> Some Hint_bt *)
    | '\x66' -> Some Op_size
    | '\x0f' -> Some Mandatory_0f
    | '\x67' -> Some Address_size
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
  let parse_disp8 a =
    (Int64.of_int (Char.code (g a)), s a)
  and parse_disp32 a =
    let r n = Int64.shift_left (Int64.of_int (Char.code (g (Int64.add a (Int64.of_int n))))) (8*n) in
    let d = r 0 in
    let d = Int64.logor d (r 1) in
    let d = Int64.logor d (r 2) in
    let d = Int64.logor d (r 3) in
    (d, (Int64.add a 4L))
  in
  let parse_sib m a =
    (* ISR 2.1.5 Table 2-3 *)
    let b = Char.code (g a) in
    let ss = b >> 6 and idx = (b>>3) & 7 in
    let base, na = if (b & 7) <> 5 then (bits2reg32e (b & 7), s a)
      else match m with _ -> unimplemented "sib ebp +? disp"
    in
    if idx = 4 then (base, na) else
      let idx = bits2reg32e idx in
      if ss = 0 then (base +* idx, na)
      else (base +* (idx <<* i32 ss), na)
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
      let (base, na) = if 4 = rm then parse_sib m na else (bits2reg32e rm, na) in
      let (disp, na) = if m = 1 then parse_disp8 na else (*2*) parse_disp32 na in
      (r, Oaddr(base +* l32 disp), na)
    | 3 -> (r, Oreg(bits2reg32 rm), s a)
    | _ -> failwith "Impossible"
  in
  let parse_modrm32 a =
    let (r, rm, na) = parse_modrm32ext a in
    (bits2reg32 r, rm, na)
  in
  let parse_imm8 a =
    let (l,na) = parse_disp8 a in
    (Oimm l, na)
  and parse_imm32 a =
    let (l,na) = parse_disp32 a in
    (Oimm l, na)
  in

  let get_opcode opsize a =
    let b1 = Char.code (g a)
    and na = s a in
    match b1 with
    | 0xc3 -> (Retn, na)
      (* FIXME: operand widths *)
    | 0x80 | 0x81 | 0x82
    | 0x83 -> let (r, rm, na) = parse_modrm32ext na in
	      (match r with (* Grp 1 *)
	      | _ -> unimplemented (Printf.sprintf "unsupported opcode: %x/%d" b1 r)
	      )
    | 0x89 -> let (r, rm, na) = parse_modrm32 na in
	      (Mov(rm, Oreg r), na)
    | 0x8b -> let (r, rm, na) = parse_modrm32 na in
	      (Mov(Oreg r, rm), na)
    | 0xc7 -> let (_, rm, na) = parse_modrm32 na in
	      let (i,na) = parse_imm32 na in
	      (Mov(rm, i), na)
    | 0xe8 -> let (i,na) = parse_imm32 na in
	      (Call(i, na), na)
    | 0xc0 | 0xc1
    | 0xd0 | 0xd1 | 0xd2
    | 0xd3 -> let (r, rm, na) = parse_modrm32ext na in
	      let opsize = if (b1 & 1) = 0 then r8 else opsize in
	      let (amt, na) = match b1 & 0xfe with
		| 0xc0 -> parse_imm8 na
		| 0xd0 -> (Oimm 1L, na)
		| 0xd2 -> (Oreg ecx, na)
		| _ -> failwith "impossible"
	      in
	      (match r with (* Grp 2 *)
	      | 4 -> (Shift(LSHIFT, opsize, rm, amt), na)
	      | 5 -> (Shift(RSHIFT, opsize, rm, amt), na)
	      | 7 -> (Shift(ARSHIFT, opsize, rm, amt), na)
	      | _ -> unimplemented "Grp 2: rolls"
	      )
						 
    | 0xff -> let (r, rm, na) = parse_modrm32ext na in
	      (match r with (* Grp 5 *)
	      | _ -> unimplemented (Printf.sprintf "unsupported opcode: ff/%d" r)
	      )
    | n -> unimplemented (Printf.sprintf "unsupported opcode: %x" n)

  in
  let pref, a = get_prefixes addr in
  let opsize = if List.mem Op_size pref then r16 else r32 in
  let op, a = get_opcode opsize a in
  let ir = ToIR.to_ir pref op in
  (ToIR.add_labels addr ir, a)
