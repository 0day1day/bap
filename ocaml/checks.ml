(* Sanity checks *)

open Cfg
module D=Debug.Make(struct let name = "Checks" and default=`Debug end)
open D

exception Sanity of string

type 'a sanityf = 'a -> string -> unit

let insane s = raise (Sanity s)

let wrapdebug f x y =
  if debug () then f x y
  else ()

module MakeConnectedCheck(C:Cfg.CFG) = struct
  module R = Reachable.Make(C)

  let connected_check g s =
    let unreachable = R.unreachable g (C.G.V.create BB_Entry) in
    match unreachable with
    | [] -> ()
    | x::[] -> insane (Printf.sprintf "%s expects a connected graph, but %s is unreachable. You should prune unreachable nodes." s (C.v2s x))
    | x::y ->
      insane (Printf.sprintf "Analysis %s expects a connected graph, but %s and %d other nodes are unreachable. You should prune unreachable nodes." s (C.v2s x) (List.length y))
  let connected_check = wrapdebug connected_check

end

let connected_astcfg = let module CC = MakeConnectedCheck(Cfg.AST) in CC.connected_check
let connected_ssacfg = let module CC = MakeConnectedCheck(Cfg.SSA) in CC.connected_check

