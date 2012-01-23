(** is_temp v returns true iff v is used as a temporary, based on its
    name.  A temporary is a variable introduced by BAP's lifting process
    that is only referenced inside one assembly block.

    The evaluator uses this information to throw away any state stored
    for these temporaries once control passes out of an assembly block.
*)
let is_temp (Var.V(_, s, t)) =
  (* First try VEX style vars *)
  ((String.length s > 2) && (String.sub s 0 2 = "T_"))
  || s = "ra"
  || s = "t1"
  || s = "t2"
  || s = "t3"
  || s = "t4"
  || s = "t5"
  || s = "t6"
  || s = "t7"
  || s = "t8"
  || s = "t9"
  || s = "t10"
  || s = "t11"
  || s = "t12"
  || s = "t13"
  || s = "t14"
  || s = "t15"
  || s = "t16"
  || s = "tmpDEST"
  || s = "origDEST"
  || s = "origCOUNT"
  || s = "src1"
  || s = "src2"
  || s = "tmp"
  || s = "t"
