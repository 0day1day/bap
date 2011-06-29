open OUnit

let var1 = Var.newvar "var1" (Type.Reg(8));;
let var2 = Var.newvar "var2" (Type.Reg(8));;

let test_newvar _ =
  assert_equal ~cmp:Var.equal ~msg:"var1 should equal var1" var1 var1;;

let suite = "Var oUnit test suite" >:::
  [
	"test_identity" >:: test_newvar;
  ]
