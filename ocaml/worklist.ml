(* XXX: Use list implementation as in graphDataFlow *)

include Queue

let add x w =
  try
    Queue.iter (fun x' -> if x = x' then raise Exit) w;
    (* We did not find x, so add it. *)
    Queue.add x w
  with Exit -> ()

let push = add

let add_list l w =
  List.iter (fun e -> add e w) l
