(* Dataflow module for use with the ocamlgraph library
   @author Ivan Jager
*)

open Util
open BatListFull
module D = Debug.Make(struct let name = "GraphDataflow" and default = `NoDebug end)
open D

module type G =
sig
  type t
  module V : Graph.Sig.COMPARABLE
  val pred : t -> V.t -> V.t list
  val succ : t -> V.t -> V.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end


(** dataflow directions *)
type direction = Forward | Backward


module type BOUNDED_MEET_SEMILATTICE =
sig
  type t
  val top : t
  val meet : t -> t -> t
  val equal : t -> t -> bool
end

module type BOUNDED_MEET_SEMILATTICE_WITH_WIDENING =
sig
  include BOUNDED_MEET_SEMILATTICE
  val widen : t -> t -> t
end

(* a dataflow is defined by a lattice over a graph. *)
module type DATAFLOW =
sig

  module L : BOUNDED_MEET_SEMILATTICE
  module G : G

  val transfer_function : G.t -> G.V.t -> L.t -> L.t
  val s0 : G.t -> G.V.t
  val init : G.t -> L.t
  val dir : direction
end

module type DATAFLOW_WITH_WIDENING =
sig

  module L : BOUNDED_MEET_SEMILATTICE_WITH_WIDENING
  module G : G

  val transfer_function : G.t -> G.V.t -> L.t -> L.t
  val s0 : G.t -> G.V.t
  val init : G.t -> L.t
  val dir : direction
end


module Make (D:DATAFLOW) = 
struct
  module H = Hashtbl.Make(D.G.V)

 let worklist_iterate ?(init = D.init) g = 
    let nodes = D.G.fold_vertex (fun x acc -> x::acc) g [] in
    let f_t = D.transfer_function g in 
    let succ,pred = match D.dir with
      | Forward ->
	  (D.G.succ g, D.G.pred g) 
      | Backward ->
	  (D.G.pred g, D.G.succ g)
    in
    let htin = H.create (List.length nodes) in
    let dfin = H.find htin in (* function to be returned *)
    let htout = H.create (List.length nodes) in
    let dfout n =
      try
	H.find htout n
      with Not_found ->
	let out = (f_t n (dfin n)) in
	H.add htout n out;
	out
    in
    List.iter (fun n -> H.add htin n D.L.top) nodes;
    H.replace htin (D.s0 g) (init g);
    let rec do_work = function
      | [] -> ()
      | b::worklist ->  
	  let inset = (dfin b) in 
	  let outset = (f_t b inset) in 
	  H.replace htout b outset;
	  let affected_elems =
	    List.filter 
	      (fun s ->
		 let oldin = dfin s in
		 let newin = D.L.meet oldin (outset) in
		 if D.L.equal oldin newin
		 then false
		 else let () = H.replace htin s newin in true
	      )
	      (succ b)
	  in
	  let newwklist = worklist@list_difference affected_elems worklist
	  in
	  do_work newwklist
    in
    do_work [D.s0 g];
    (dfin, dfout)

end

module MakeWide (D:DATAFLOW_WITH_WIDENING) = 
struct
  include Make(D)

 let worklist_iterate_widen ?(init = D.init) g = 
    let nodes = D.G.fold_vertex (fun x acc -> x::acc) g [] in
    let f_t = D.transfer_function g in 
    let succ,pred = match D.dir with
      | Forward ->
	  (D.G.succ g, D.G.pred g) 
      | Backward ->
	  (D.G.pred g, D.G.succ g)
    in
    let htin = H.create (List.length nodes) in
    let dfin = H.find htin in (* function to be returned *)
    let htout = H.create (List.length nodes) in
    let dfout n =
      try
	H.find htout n
      with Not_found ->
	let out = (f_t n (dfin n)) in
	H.add htout n out;
	out
    in
    let visited = H.create (List.length nodes) in
    let backedge = Hashtbl.create (List.length nodes) in
    let is_backedge s d =
      try Hashtbl.find backedge (s,d)
      with Not_found ->
        (* self loop, or we visited the destination edge already *)
        let v = D.G.V.equal s d || (H.mem visited s = false && H.mem visited d = true) in
        Hashtbl.add backedge (s,d) v;
        v
    in
    List.iter (fun n -> H.add htin n D.L.top) nodes;
    H.replace htin (D.s0 g) (init g);
    let rec do_work = function
      | [] -> ()
      | b::worklist ->  
	  let inset = (dfin b) in 
	  let outset = (f_t b inset) in 
	  H.replace htout b outset;
	  let affected_elems =
	    List.filter 
	      (fun s ->
		 let oldin = dfin s in
		 let newin = D.L.meet oldin outset in
		 if D.L.equal oldin newin
		 then false
		 else
                   let newin =
                     if is_backedge b s
                     then (dprintf "widening"; D.L.widen oldin outset)
                     else newin
                   in
                   let () = H.replace htin s newin in
                   true)
	      (succ b)
	  in
          (* Note we must mark b as visited after we look at the
             affected elements, because we look for back edges there. *)
          H.replace visited b ();
          dprintf "visited";
	  let newwklist = worklist@list_difference affected_elems worklist
	  in
	  do_work newwklist
    in
    do_work [D.s0 g];
    (dfin, dfout)

end
