(** is_temp v returns true iff v is used as a temporary, based on its
    name.  A temporary is a variable introduced by BAP's lifting process
    that is only referenced inside one assembly block.

    The evaluator uses this information to throw away any state stored
    for these temporaries once control passes out of an assembly block.
*)
let is_temp (Var.V(_, s, t)) =
  (* First try VEX style vars *)
  s.[0] = 't'
  || ((String.length s > 2) && (String.sub s 0 2 = "T_"))
