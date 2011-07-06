open OUnit


let test_input_fails _ =
  assert_raises (Arg.Bad("No input specified")) (fun _ -> Input.get_program());;

let test_input_nop () = 
  let p = Asmir.open_program "./x86/nop" in
  let stmts = Asmir.asmprogram_to_bap p in
  match stmts with
	| stmt::stmts -> 
	  todo ("First stmt of program ./x86/nop is "^Pp.ast_stmt_to_string(stmt))
	| _ -> assert_failure 
	  "asmprogram_to_bap of open_program \"./x86/nop\" returned an empty list!"


let suite = "IL OUnit test suite" >::: 
  [
	"test_input_fails" >:: test_input_fails;
	"test_input_nop" >:: test_input_nop;
  ]
