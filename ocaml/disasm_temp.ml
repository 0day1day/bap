(** is_temp v returns true iff v is used as a temporary, based on its
    name.  A temporary is a variable introduced by BAP's lifting process
    that is only referenced inside one assembly block.

    The evaluator uses this information to throw away any state stored
    for these temporaries once control passes out of an assembly block.
*)

let temp_prefix = "T_"
let temp_prefix_len = String.length temp_prefix

let is_temp_name s =
  (* First try VEX style vars *)
  (String.length s > temp_prefix_len) && (String.sub s 0 2 = temp_prefix)

let is_temp (Var.V(_, s, _)) =
  is_temp_name s

(** Create a new temporary

    Makes sure that the temporary name is found by Disasm.is_temp.
    Always use this for making temporaries.
*)
let nt s t =
  if (is_temp_name s) then
    Var.newvar s t
  else
    let newname = temp_prefix^s in
    let () = assert (is_temp_name newname) in
    Var.newvar (newname) t
