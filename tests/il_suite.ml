open OUnit


let test_input_fails _ =
  assert_raises (Arg.Bad("No input specified")) (fun _ -> Input.get_program());;

let test_input () = todo "test_input not implemented yet";;



let suite = "IL OUnit test suite" >::: 
  [
	"test_input_fails" >:: test_input_fails;
	"test_input" >:: test_input;
  ]
