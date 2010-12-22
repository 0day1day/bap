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


   In order to get the most common unspported opcodes, you can run something like:
   for f in bin/*; do BAP_DEBUG_MODULES=AsmirV ~/bap/trunk/utils/iltrans -bin $f ; done 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n

*)

type segment = CS | SS | DS | ES | FS | GS

type operand =
  | Oreg of int
  | Oaddr of Ast.exp
  | Oimm of int64

type opcode =
  | Retn
  | Nop
  | Mov of typ * operand * operand (* dst, src *)
  | Movdqa of operand * operand (* dst, src *)
  | Lea of operand * Ast.exp
  | Call of operand * int64 (* Oimm is relative, int64 is RA *)
  | Shift of binop_type * typ * operand * operand
  | Jump of operand
  | Jcc of operand * Ast.exp
  | Hlt
  | Cmps of typ
  | Stos of typ
  | Push of typ * operand
  | Pop of typ * operand
  | Sub of typ * operand * operand
  | Cmp of typ * operand * operand
  | And of typ * operand * operand
  | Test of typ * operand * operand
  | Cld

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


let unimplemented s  = failwith ("disasm_i386: unimplemented feature: "^s)

let (&) = (land)
and (>>) = (lsr)
and (<<) = (lsl)


(* register widths *)
let r1 = Ast.reg_1
let r4 = Reg 4
let r8 = Ast.reg_8
let r16 = Ast.reg_16
let r32 = Ast.reg_32
let r64 = Ast.reg_64
let addr_t = r32
let xmm_t = TMem r4 (* 128 bits that can be accessesed as different things *)

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

and dflag = nv "R_DFLAG" r32 (* 1 if DF=0 or -1 if DF=1 *)

and fs_base = nv "R_FS_BASE" r32
and gs_base = nv "R_GS_BASE" r32


let xmms = Array.init 8 (fun i -> nv (Printf.sprintf "XMM%d" i) xmm_t)

let regs : var list =
  ebp::esp::esi::edi::eip::eax::ebx::ecx::edx::eflags::cf::pf::af::zf::sf::oF::dflag::fs_base::gs_base::
  List.map (fun (n,t) -> Var.newvar n t)
    [

  (* VEX left-overs from calc'ing condition flags *)
  ("R_CC_OP", reg_32);
  ("R_CC_DEP1", reg_32);
  ("R_CC_DEP2", reg_32);
  ("R_CC_NDEP", reg_32);

  (* more status flags *)
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
    @ Array.to_list xmms



let o_eax = Oreg 0
and o_ecx = Oreg 1
and o_edx = Oreg 2
and o_ebx = Oreg 3
and o_esp = Oreg 4
and o_ebp = Oreg 5
and o_esi = Oreg 6
and o_edi = Oreg 7

let esp_e = Var esp
and esi_e = Var esi
and edi_e = Var edi
and ecx_e = Var ecx

let mem = nv "mem" (TMem(r32))
let mem_e = Var mem
and cf_e = Var cf
and pf_e = Var pf
and af_e = Var af
and zf_e = Var zf
and sf_e = Var sf
and of_e = Var oF

and dflag_e = Var dflag

let esiaddr = Oaddr esi_e
and ediaddr = Oaddr edi_e

let seg_cs = None
and seg_ss = None
and seg_ds = None
and seg_es = None
and seg_fs = Some fs_base
and seg_gs = Some gs_base

(* exp helpers *)

let binop op a b = match (a,b) with
  | (Int(a, at), Int(b, bt)) when at = bt ->
    let (i,t) = Arithmetic.binop op (a,at) (b,bt) in
    Int(i,t)
  | _ -> BinOp(op, a, b)

let (+*) a b   = binop PLUS a b
let (-*) a b   = binop MINUS a b
let ( ** ) a b   = binop TIMES a b
let (<<*) a b  = binop LSHIFT a b
let (>>*) a b  = binop RSHIFT a b
let (>>>*) a b = binop ARSHIFT a b
let (&*) a b   = binop AND a b
let (|*) a b   = binop OR a b
let (^*) a b   = binop XOR a b
let (=*) a b   = binop EQ a b
let (<*) a b   = binop LT a b
let (>*) a b   = binop LT b a

let cast_low t e = Cast(CAST_LOW, t, e)
let cast_high t e = Cast(CAST_HIGH, t, e)
let cast_unsigned t e = Cast(CAST_UNSIGNED, t, e)

let loadm m t a =
  Load(Var m, a, little_endian, t)

let load_s s t a = match s with
  | None -> Load(mem_e, a, little_endian, t)
  | Some v -> Load(mem_e, Var v +* a, little_endian, t)

let ite t b e1 e2 = (* FIXME: were we going to add a native if-then-else thing? *)
  if t = r1 then
    (b &* e1) |*  (exp_not b &* e2) 
  else
    (Cast(CAST_SIGNED, t, b) &* e1) |*  (Cast(CAST_SIGNED, t, exp_not b) &* e2) 

let l32 i = Int(Arithmetic.to64 (i,r32), r32)
let i32 i = Int(Int64.of_int i, r32)


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

(*
let bits2reg8= function
  | 0 -> al
  | 1 -> cl
  | 2 -> dl
  | 3 -> bl
  | 4 -> ah
  | 5 -> ch
  | 6 -> dh
  | 7 -> bh
*)

and reg2bits r = Util.list_firstindex [eax; ecx; edx; ebx; esp; ebp; esi; edi] ((==)r) 

let bits2reg32e b = Var(bits2reg32 b)

let bits2reg16e b = cast_low r16 (bits2reg32e b)

let bits2reg8e b =
  if b < 4 then
    cast_low r8 (bits2reg32e b)
  else
    cast_high r8 (cast_low r16 (bits2reg32e (b land 3)))
    

let bits2xmm b = xmms.(b)
  
let reg2xmm r =
  bits2xmm (reg2bits r)


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
  | Oreg r when t = r32 -> bits2reg32e r
  | Oreg r when t = r16 -> bits2reg16e r
  | Oreg r when t = r8 -> bits2reg8e r
  | Oreg r -> unimplemented "sub registers" (*cast_low t (Var r)*)
  | Oaddr e -> load_s ss t e
  | Oimm i -> Int(Arithmetic.to64 (i,t), t)

let assn_s s t v e =
  match v with
  | Oreg r when t = r32 -> move (bits2reg32 r) e
  | Oreg r when t = r16 ->
    let v = bits2reg32 r in
    move v ((Var v &* l32 0xffff0000L) |* cast_unsigned r32 e)
  | Oreg r when t = r8 && r < 4 ->
    let v = bits2reg32 r in
    move v ((Var v &* l32 0xffffff00L) |* cast_unsigned r32 e)
  | Oreg r when t = r8 ->
    let v = bits2reg32 (r land 3) in
    move v ((Var v &* l32 0xffff00ffL) |* (cast_unsigned r32 e <<* i32 8))
  | Oreg _ -> unimplemented "assignment to sub registers"
  | Oaddr a -> store_s s t a e
  | Oimm _ -> failwith "disasm_i386: Can't assign to an immediate value"

let bytes_of_width = function
  | Reg x when x land 7 = 0 -> x/8
  | _ -> failwith "bytes_of_width"

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
  cjmp (ecx_e =* l32 0L) (l32 next)
  @ move ecx (ecx_e -* i32 1)
  :: stmts
  @ [endstmt]
 
let reta = [StrAttr "ret"]
and calla = [StrAttr "call"]

let compute_sf result = Cast(CAST_HIGH, r1, result)
let compute_zf t result = Int(0L, t) =* result
let compute_pf r =
  (* extra parens do not change semantics but do make it pretty print nicer *)
  exp_not (Cast(CAST_LOW, r1, (((((((r >>* i32 7) ^* (r >>* i32 6)) ^* (r >>* i32 5)) ^* (r >>* i32 4)) ^* (r >>* i32 3)) ^* (r >>* i32 2)) ^* (r >>* i32 1)) ^* r))

let set_sf r = move sf (compute_sf r)
let set_zf t r = move zf (compute_zf t r)
let set_pf r = move pf (compute_pf r)

let set_pszf t r =
  [set_pf r;
   set_sf r;
   set_zf t r]

let set_flags_sub t s1 s2 r =
  set_sf r
  ::set_zf t r
  ::set_pf r
  ::move cf (r >* s1)
  ::move af ((r &* Int(7L,t)) <* (s1 &* Int(7L,t))) (* Is this right? *)
  ::move oF (Cast(CAST_HIGH, r1, (s1 ^* s2) &* (s1 ^* r) ))
  ::[]
    

let to_ir addr next ss pref =
  let load = load_s ss (* Need to change this if we want seg_ds <> None *)
  and op2e = op2e_s ss
  and store = store_s ss
  and assn = assn_s ss in
  function
  | Nop -> []
  | Retn when pref = [] ->
    let t = nv "ra" r32 in
    [move t (load_s seg_ss r32 esp_e);
     move esp (esp_e +* (i32 4));
     Jmp(Var t, [StrAttr "ret"])
    ]
  | Mov(t, dst,src) when pref = [] ->
    [assn t dst (op2e t src)]
  | Movdqa(d,s) -> (
    let zero = Int(0L,r4) and eight = Int(8L,r4) in
    let (s0, s1, a1) = match s with
      | Oreg i -> let r = bits2xmm i in
		  (loadm r r64 zero, loadm r r64 eight, [])
      | Oaddr a -> (load r64 a, load r64 (a +* Int(8L, addr_t)), [a])
      | Oimm _ -> failwith "invalid"
    in
    let (d0, d1, a2) = match d with
      (* FIXME: should this be a single move with two stores? *)
      | Oreg i -> let r = bits2xmm i in
		  (storem r r64 zero s0, storem r r64 eight s1, [])
      | Oaddr a -> (store r64 a s0, store r64 (a +* Int(8L, addr_t)) s1, [a])
      | Oimm _ -> failwith "invalid"
    in
    (List.map (fun a -> Assert( (a &* i32 15) =* i32 0, [])) (a1@a2))
    @ [d0;d1;]

  )
  | Lea(r, a) when pref = [] ->
    [assn r32 r a]
  | Call(Oimm i, ra) when pref = [] ->
    [move esp (esp_e -* i32 4);
     store r32 esp_e (l32 ra);
     Jmp(l32(Int64.add i ra), calla)]
  | Jump(o) ->
    [ Jmp(op2e r32 o, [])]
  | Jcc(o, c) ->
    cjmp c (op2e r32 o)
  | Shift(st, s, o1, o2) (*when pref = [] || pref = [pref_opsize]*) ->
    assert (List.mem s [r8; r16; r32]);
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
       move t1 (e1 >>* (count -* i32 1));
     move cf (ifzero cf_e (Cast(CAST_LOW, r1, Var t1)));
     assn s o1 (s_f e1 count);
     move oF (ifzero of_e (ite r1 (count =* i32 1) (our_of) (Unknown("OF <- undefined", r1))));
     move sf (ifzero sf_e (compute_sf e1));
     move zf (ifzero zf_e (compute_zf s e1));
     move pf (ifzero pf_e (compute_pf e1))
    ]
  | Hlt ->
    [Jmp(Lab "General_protection fault", [])]
  | Cmps(Reg bits as t) ->
    let src1 = nv "src1" t and src2 = nv "scr2" t and tmpres = nv "tmp" t in
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
    let tmp = nv "t" t in (* only really needed when o involves esp *)
    move tmp (op2e t o)
    :: move esp (esp_e -* i32 (bytes_of_width t))
    :: store_s seg_ss t esp_e (Var tmp) (* FIXME: can ss be overridden? *)
    :: []
  | Pop(t, o) ->
    [assn t o (load_s seg_ss t esp_e);
     move esp (esp_e +* i32 (bytes_of_width t)) ]
  | Sub(t, o1, o2) ->
    let tmp = nv "t" t in
    move tmp (op2e t o1)
    :: assn t o1 (op2e t o1 -* op2e t o2)
    :: set_flags_sub t (Var tmp) (op2e t o2) (op2e t o1)
  | Cmp(t, o1, o2) ->
    let tmp = nv "t" t in
    move tmp (op2e t o1 -* op2e t o2)
    :: set_flags_sub t (op2e t o1) (op2e t o2) (Var tmp)
  | And(t, o1, o2) ->
    assn t o1 (op2e t o1 &* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after and", r1))
    :: set_pszf t (op2e t o1)
  | Test(t, o1, o2) ->
    let tmp = nv "t" t in
    move tmp (op2e t o1 &* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after and", r1))
    :: set_pszf t (Var tmp)
  | Cld ->
    [Move(dflag, i32 1, [])]
  | _ -> unimplemented "to_ir"

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

  let opr = function
    | Oreg v -> unimplemented "rewrite this" (*Var.name v*)
    | Oimm i -> Printf.sprintf "$0x%Lx" i
    | Oaddr a -> Pp.ast_exp_to_string a

  let op2str = function
    | Retn -> "ret"
    | Nop -> "nop"
    | Mov(t, d,s) -> Printf.sprintf "mov %s, %s" (opr d) (opr s)
    | Lea(r,a) -> Printf.sprintf "lea %s, %s" (opr r) (opr (Oaddr a))
    | Call(a, ra) -> Printf.sprintf "call %s" (opr a)
    | Shift _ -> "shift"
    | Hlt -> "hlt"
    | Jump a -> Printf.sprintf "jmp %s" (opr a)
    | _ -> unimplemented "op2str"

  let to_string pref op =
    failwith "fallback to libdisasm"
    (* prefs2str pref ^ op2str op *)
end (* ToStr *)

(* extract the condition to jump on from the opcode bits
for 70 to 7f and 0f 80 to 8f *)
let cc_to_exp i =
  let cc = match i & 0xe with
    | 0x2 -> cf_e
    | 0x4 -> zf_e
    | 0x6 -> cf_e |* zf_e
    | 0x8 -> sf_e
    | _ -> failwith "unsupported condition code"
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
  let parse_disp8 a =
    (Arithmetic.tos64 (Int64.of_int (Char.code (g a)), r8), s a)
  and parse_disp32 a =
    let r n = Int64.shift_left (Int64.of_int (Char.code (g (Int64.add a (Int64.of_int n))))) (8*n) in
    let d = r 0 in
    let d = Int64.logor d (r 1) in
    let d = Int64.logor d (r 2) in
    let d = Int64.logor d (r 3) in
    (d, (Int64.add a 4L))
  in
  let parse_disp = function
    | Reg 8 ->  parse_disp8
    | Reg 16 -> unimplemented "16-bit displacement"
    | Reg 32 -> parse_disp32
    | _ -> failwith "unsupported displacement size"
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
    | 3 -> (r, Oreg rm, s a)
    | _ -> failwith "Impossible"
  in
  let parse_modrm32 a =
    let (r, rm, na) = parse_modrm32ext a in
    (Oreg r, rm, na)
(*  and parse_modrmxmm a =
    let (r, rm, na) = parse_modrm32ext a in
    let rm = match rm with Oreg r -> Oreg (reg2xmm r) | _ -> rm in
    (Oreg(bits2xmm r), rm, na) *)
  in
  let parse_modrm opsize a = match opsize with
    | Reg 32 -> parse_modrm32 a
    | _ -> unimplemented "modrm other than 32"
  in
  let parse_imm8 a = (* not sign extended *)
    (Oimm(Int64.of_int (Char.code (g a))), s a)
  and parse_simm8 a = (* sign extended *)
    let (d, na) = parse_disp8 a in
    (Oimm d, na)
  and parse_imm32 a =
    let (l,na) = parse_disp32 a in
    (Oimm l, na)
  in
  let parse_immz t a = match t with
    | Reg 16 -> failwith "parse_imm16 a"
    | Reg 32 | Reg 64 -> parse_imm32 a
    | _ -> failwith "parse_immz unsupported size"
  in
  let parse_immv = parse_immz in (* until we do amd64 *)
  let get_opcode pref opsize a =
    let b1 = Char.code (g a)
    and na = s a in
    match b1 with (* Table A-2 *)
    | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 ->
      (Push(opsize, Oreg(b1 & 7)), na)
    | 0x58 | 0x59 | 0x5a | 0x5b | 0x5c | 0x5d | 0x5e | 0x5f ->
      (Pop(opsize, Oreg(b1 & 7)), na)
    | 0x68 (* | 0x6a *) ->
      let (o, na) = if b1=0x68 then parse_immz opsize na else parse_simm8 na in
      (Push(opsize, o), na)
    | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78
    | 0x79 -> let (i,na) = parse_disp8 na in
	      (Jcc(Oimm(Int64.add i na), cc_to_exp b1), na)
    | 0xc3 -> (Retn, na)
      (* FIXME: operand widths *)
    | 0x80 | 0x81 | 0x82
    | 0x83 -> let (r, rm, na) = parse_modrm32ext na in
	      let (o2, na) =
		(* for 0x83, imm8 needs to be sign extended *)
		if b1 = 0x81 then parse_immz opsize na else parse_simm8 na
	      in
	      let opsize = if b1 land 1 = 0 then r8 else opsize in
	      (match r with (* Grp 1 *)
	      | 4 -> (And(opsize, rm, o2), na)
	      | 5 -> (Sub(opsize, rm, o2), na)
	      | 7 -> (Cmp(opsize, rm, o2), na)
	      | _ -> unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
	      )
    | 0x84
    | 0x85 -> let (r, rm, na) = parse_modrm32 na in
	      let o = if b1 = 0x84 then r8 else opsize in
	      (Test(o, rm, r), na)
    | 0x89 -> let (r, rm, na) = parse_modrm32 na in
	      (Mov(opsize, rm, r), na)
    | 0x8b -> let (r, rm, na) = parse_modrm32 na in
	      (Mov(opsize, r, rm), na)
    | 0x8d -> let (r, rm, na) = parse_modrm opsize na in
	      (match rm with
	      | Oaddr a -> (Lea(r, a), na)
	      | _ -> failwith "invalid lea (must be address)"
	      )
    | 0x90 -> (Nop, na)
    | 0xa1 -> let (addr, na) = parse_disp32 na in
	      (Mov(opsize, o_eax, Oaddr(l32 addr)), na)
    | 0xa3 -> let (addr, na) = parse_disp32 na in
	      (Mov(opsize, Oaddr(l32 addr), o_eax), na)
    | 0xa6 -> (Cmps r8, na)
    | 0xa7 -> (Cmps opsize, na)
    | 0xaa -> (Stos r8, na)
    | 0xab -> (Stos opsize, na)
    | 0xb8 | 0xb9 | 0xba | 0xbb | 0xbc | 0xbd | 0xbe
    | 0xbf -> let (i, na) = parse_immv opsize na in
	      (Mov(opsize, Oreg(b1 & 7), i), na)
    | 0xc7 -> let (_, rm, na) = parse_modrm32 na in
	      let (i,na) = parse_immz opsize na in
	      (Mov(opsize, rm, i), na)
    | 0xe8 -> let (i,na) = parse_disp32 na in
	      (Call(Oimm i, na), na)
    | 0xe9 -> let (i,na) = parse_disp opsize na in
	      (Jump(Oimm(Int64.add i na)), na)
    | 0xeb -> let (i,na) = parse_disp8 na in
	      (Jump(Oimm(Int64.add i na)), na)
    | 0xc0 | 0xc1
    | 0xd0 | 0xd1 | 0xd2
    | 0xd3 -> let (r, rm, na) = parse_modrm32ext na in
	      let opsize = if (b1 & 1) = 0 then r8 else opsize in
	      let (amt, na) = match b1 & 0xfe with
		| 0xc0 -> parse_imm8 na
		| 0xd0 -> (Oimm 1L, na)
		| 0xd2 -> (o_ecx, na)
		| _ -> failwith "impossible"
	      in
	      (match r with (* Grp 2 *)
	      | 4 -> (Shift(LSHIFT, opsize, rm, amt), na)
	      | 5 -> (Shift(RSHIFT, opsize, rm, amt), na)
	      | 7 -> (Shift(ARSHIFT, opsize, rm, amt), na)
	      | _ -> unimplemented "Grp 2: rolls"
	      )
    | 0xf4 -> (Hlt, na)
    | 0xfc -> (Cld, na)
    | 0xff -> let (r, rm, na) = parse_modrm32ext na in
	      (match r with (* Grp 5 *)
	      | 6 -> (Push(opsize, rm), na)
	      | _ -> unimplemented (Printf.sprintf "unsupported opcode: ff/%d" r)
	      )
    | 0x0f -> (
      let b2 = Char.code (g na) and na = s na in
      match b2 with (* Table A-3 *)
      | 0x6f | 0x7f when pref = [0x66] -> (
	let r, rm, na = parse_modrm32 na in
	let s,d = if b2 = 0x6f then rm, r else r, rm in
	(Movdqa(d,s), na)
      )
      | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88
      | 0x89 ->	let (i,na) = parse_disp32 na in
		(Jcc(Oimm(Int64.add i na), cc_to_exp b2), na)
      | _ -> unimplemented (Printf.sprintf "unsupported opcode: %02x %02x" b1 b2)
    )
    | n -> unimplemented (Printf.sprintf "unsupported opcode: %02x" n)

  in
  let pref, a = get_prefixes addr in
  let opsize = if List.mem pref_opsize pref then r16 else r32 in
  let op, a = get_opcode pref opsize a in
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
  let asm = try Some(ToStr.to_string pref op) with Failure _ -> None in
  (ToIR.add_labels ?asm addr ir, na)


