open OUnit

let open_program_test _ = 
  ignore (Asmir.open_program ~loud:false "x86/nop");;

let suite = "Asmir" >:::
  [
	"open_program_test" >:: open_program_test;
  ]
