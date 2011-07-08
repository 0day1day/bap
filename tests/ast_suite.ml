open OUnit

(*let exp_var = Ast.Exp(Var.newvar(reg_32));;*)

let test_truth_id _ = 
  let s = "Ast.exp_true does not evaluate to true!" in
  assert_bool s	(Eval.is_true Ast.exp_true);;

let test_false_id _ = 
  let s = "Ast.exp_true does not evaluate to true!" in
  assert_bool s (not(Eval.is_false Ast.exp_true));;


let suite = "Ast OUnit test suite" >::: 
  [
	"test_truth_id" >:: test_truth_id;
	"test_false_id" >:: test_false_id;
  ]

