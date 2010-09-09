open Int64
open Ast
open Type

type segment = CS | SS | DS | ES | FS | GS
type prefix =
  | Lock | Repnz | Repz
  | Override of segment | Hint_bnt | Hint_bt
  | Op_size | Mandatory_0f
  | Address_size

type opcode =
  | Retn

let unimplemented() = failwith "disasm_i386: unimplemented feature"

(* register widths *)
let r32 = Ast.reg_32

(* registers *)

let regs : var list =
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


let esp = List.nth regs 1
let e_esp = Var esp

let mem = Var.newvar "mem" (TMem(reg_32))
let e_mem = Var mem

(* exp helpers *)
let load t a =
  Load(e_mem, a, little_endian, t)

let (+*) a b = BinOp(PLUS, a, b)

let i32 i = Int(Int64.of_int i, r32)


(* stmt helpers *)
let assn v e =
  Move(v, e, [])

let opcode2ir pref = function
  | Retn when pref = [] ->
    let t = Var.newvar "ra" r32 in
    [assn t (load r32 e_esp);
     assn esp (e_esp +* (i32 4));
     Jmp(Var t, [StrAttr "ret"])
    ]
  | _ -> unimplemented()

let add_labels a ir =
  Label(Addr a,[])
  ::Label(Name(Printf.sprintf "pc_0x%Lx" a),[])
  ::ir


let disasm_instr g addr =
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
      | Some p -> f (p::l) (Int64.succ a)
      | None -> (l, a)
    in
    f [] a
  in
 
  let get_opcode a = match Char.code (g a) with
    | 0xc3 -> (Retn, Int64.succ a)
    | _ -> unimplemented()

  in
  let pref, a = get_prefixes addr in
  let op, a = get_opcode a in
  let ir = opcode2ir pref op in
  add_labels addr ir
