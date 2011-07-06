open OUnit


let parse_nop _ = 
  let nop _ = Char.chr(Int64.to_int(Int64.of_string("0x90"))) in
  let (l,op,i64) = Disasm_i386.parse_instr (nop) (Int64.zero) in
  assert_equal 
	~msg:("instruction: " ^ Disasm_i386.ToStr.op2str(op) ^ " is not a Nop!") 
	Disasm_i386.Nop op;;

let suite = "Disasm_i386 OUnit test suite" >:::
  [
	"parse_nop" >:: parse_nop;
  ]
