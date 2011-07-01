open OUnit

let open_program_test _ = 
  let p = Asmir.open_program "x86/nop" in
  todo "Implement open_program test";;

let suite = "Asmir OUnit test suite" >:::
  [
	"open_program_test" >:: open_program_test;
  ]
