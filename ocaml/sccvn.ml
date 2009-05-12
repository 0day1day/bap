(** Strongly connected component based value numbering.
    
    Currently we only implement the RPO algorithm, described in
    "SCC-Based Value Numbering" by Keith Cooper and Taylor Simpson.
    http://citeseer.ist.psu.edu/41805.html

    @author Ivan Jager
*)

open Type
open Ssa
open ExtList

module VH = Var.VarHash
module C = Cfg.SSA
module G = C.G

module D = Debug.Make(struct let name = "SCCVN" and default=`NoDebug end)
open D

module Dom = Dominator.Make(G)
    



type hash = Top | Hash of Ssa.var
let top = Top
type expid = 
  | Const of Ssa.value (* Except Var *)
  | Bin of binop_type * hash * hash
  | Un of unop_type * hash
  | Cst of cast_type * typ * hash
  | Unique of var
  | Ld of hash * hash * hash * typ
  | St of hash * hash * hash * hash * typ
  | Ph of hash list
      
type rpoinfo = { (* private to the SCCVN module *)
  vn_h : hash VH.t; (* maps vars to value numbers *)
  eid2vn : (expid, hash) Hashtbl.t; (* maps expids to value numbers *)
  vn2eid : (hash, expid) Hashtbl.t; (* inverse of eid2vn *)
}


(** [node_sdom cfg a b] returns true if position [a] strictly dominates
    position [b]. (where a position is a bbid * distance into the BB) *)
let pos_sdom cfg =
  let {Dom.sdom=sdom;} = Dom.compute_all cfg (G.V.create Cfg.BB_Entry) in
  (fun (a_bb, a_i) (b_bb, b_i) ->
     sdom a_bb b_bb || (a_bb = b_bb && a_i < b_i)
  )

  

let defsite cfg =
  let defsites = VH.create 5700 in
  G.iter_vertex
    (fun b ->
       let rec addone i = function
	 | Move(l,_,_) -> VH.add defsites l (b,i)
	 | _ -> ()
	 in
       List.iteri addone (C.get_stmts cfg b)
    )
    cfg;
  let beforeentry = (C.G.V.create Cfg.BB_Entry, -1) in
  (fun x -> 
     try VH.find defsites x
     with Not_found -> beforeentry (* globals come from before BB_Entry *)
  )




    
let add_eid =
  let name = "fake var for constant"
  and typ = Array(TMem REG_1, TMem REG_1) (* BS type *) in
  (fun info eid ->
     let h = Hash(Var.newvar name typ) in
     Hashtbl.add info.eid2vn eid h;
     Hashtbl.add info.vn2eid h eid;
     h )

let add_hash info var eid =
  let h = Hash var in
  Hashtbl.add info.eid2vn eid h;
  Hashtbl.add info.vn2eid h eid;
  VH.replace info.vn_h var h;
  h

let get_expid info =
  let vn = function
    | (Int _ | Lab _) as v -> (
	try Hashtbl.find info.eid2vn (Const v)
	with Not_found ->
	  add_eid info (Const v)
      )
    | Var x -> (
	try VH.find info.vn_h x
	with Not_found ->
	  failwith("get_expid: unknown var: "^Pp.var_to_string x)
      )
  in
  fun var -> function
    | Val(Var _ as v) ->
	Hashtbl.find info.vn2eid (vn v) 
    | Val v -> Const v
    | BinOp((PLUS|TIMES|AND|OR|XOR|EQ|NEQ) as op,v1,v2) ->
	let (h1,h2) = (vn v1, vn v2) in
	(* FIXME: Should this be h1 < h2? *)
	if v1 < v2 then Bin(op, h1, h2) else Bin(op, h2, h1)
    | BinOp(op,v1,v2) -> Bin(op, vn v1, vn v2)
    | UnOp(op, v) -> Un(op, vn v)
    | Cast(ct, t, v) -> Cst(ct,t, vn v)
    | Unknown _ -> Unique var
    | Load(m,i,e,t) -> Ld(vn m, vn i, vn e, t)
    | Store(m,i,v,e,t) -> St(vn m, vn i, vn v, vn e, t)
    | Phi vars -> Ph(List.map (fun v -> vn (Var v)) vars)
	
let lookup info var exp =
  try
    let eid = get_expid info var exp in
    try Hashtbl.find info.eid2vn eid
    with Not_found ->
      match eid with
      | Const(Var _) -> top
      | _ ->
	  add_hash info var eid
  with Not_found -> (* no VNs for subexpressions yet *)
    top
      


module Dfs = Graph.Traverse.Dfs(G)

let fold_postfix_component f g v i=
  let acc = ref i in
  Dfs.postfix_component (fun x -> acc := f x !acc) g v;
  !acc

    
let rpo cfg =
  let info = {
    vn_h = VH.create 57;
    eid2vn = Hashtbl.create 57;
    vn2eid = Hashtbl.create 57;
  }
  in
  (* Contrary to the paper, only assigned SSA variables should have
     their hashes set to Top. Otherwise, uninitialized variables are
     all equivalent. *)
  let filter l = function
    | Move(v,e, _) ->
	VH.add info.vn_h v top;
	(v,e)::l
    | _ -> l
  in
  let moves = (* extract the moves only once *)
    fold_postfix_component
      (fun b l ->
	 List.fold_left filter l (List.rev(C.get_stmts cfg b))
      )
      cfg (C.G.V.create Cfg.BB_Entry) []
  in
  let () = (* add all other uninitialized vars as unique *)
    let vis = object
      inherit Ssa_visitor.nop
      method visit_rvar x =
	if not(VH.mem info.vn_h x) then (
	  dprintf "Adding uninitialized variable %s" (Pp.var_to_string x);
	  ignore(add_hash info x (Unique x));
	);
	`DoChildren
    end
    in
    C.G.iter_vertex
    (fun b -> ignore(Ssa_visitor.stmts_accept vis (C.get_stmts cfg b)))
    cfg;
  in
  let vn x = 
    try VH.find info.vn_h x
    with Not_found -> failwith("vn: Unknown var: "^Pp.var_to_string x)
  in
  let lookup = lookup info in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (v,e) ->
	 let oldvn = vn v in
	 let temp = lookup v e in
	 if oldvn <> temp && temp <> top then (
	   changed := true;
	   VH.replace info.vn_h v temp
	 ) )
      moves
  done;
  (******** END OF ALGORITHM FROM PAPER ******)
  let inverse = Hashtbl.create (VH.length info.vn_h) in
  let () = VH.iter (fun k v -> Hashtbl.add inverse v k) info.vn_h in
  let hash2equiv = Hashtbl.find_all inverse in
  let vn2eid = Hashtbl.find info.vn2eid in
  (*	let () =
	if debug then (
	List.iter
	(fun (v,_) -> pdebug (List.fold_left (fun s v -> s^var_to_string v^" ") "[" (hash2equiv(vn v)) ^"]"))
	moves
	)
	in *)
  (vn,hash2equiv,vn2eid)


let hash_replacement hash2equiv vn2eid defsite psdom =
  let remove_dominated vars =
    let lt (_,d) (_,d') = psdom d d' in
    let rec extract_roots found = function
      | [] -> found
      | first::rest ->
	  let (min,rest) =
	    List.fold_left
	      (fun (m,r) x -> if lt m x then (m,x::r) else (x,m::r))
	      (first,[]) rest
	  in
	  if List.exists (fun x -> lt x min) found then
	    found
	  else
	    extract_roots (min::found) rest
    in
    let var_defsites = List.rev_map (fun x -> (x, defsite x)) vars in
    List.map fst (extract_roots [] var_defsites)
  in
  (* cache the variables that are not dominated by an equivalent variable *)
  let myequiv_ht = Hashtbl.create 5700 in
  let hash2equiv x =
    try Hashtbl.find myequiv_ht x
    with Not_found ->
      let res = remove_dominated (hash2equiv x) in
      Hashtbl.add myequiv_ht x res;
      res
  in
  fun pos hash ->
    let rec find_best p rest =
      match rest with
      | [] -> None
      | v'::tl ->
	  let p' = defsite v' in
	  if psdom p' p then
	    Some v'
	  else find_best p tl
    in
    match vn2eid hash with
    | Unique v when psdom (defsite v) pos ->
	Some(Var v)
    | Const c ->
	Some c
    | _ ->
	match find_best pos (hash2equiv hash) with
	| Some v -> Some(Var v)
	| None -> None


let replacer cfg =
  let () = pdebug "Running rpo algorithm" in
  let (vn,hash2equiv,vn2eid) = rpo cfg in
  let () = pdebug "Compting dominators" in
  let psdom = pos_sdom cfg in
  let () = pdebug "Computing defsites" in
  let defsite = defsite cfg in
  let hash_replacement = hash_replacement hash2equiv vn2eid defsite psdom in
  let changed = ref false in
  let vis = object
    inherit Ssa_visitor.nop
    val mutable pos = (C.G.V.create Cfg.BB_Entry, 0)
    method set_pos p = pos <- p
    method visit_value = function
      | Ssa.Var v ->
	  (match hash_replacement pos (vn v) with
	   | Some(Ssa.Var var) when v == var -> `SkipChildren
	   | Some v' ->
	       changed := true;
	       dprintf "Replacing var %s with %s" (Pp.var_to_string v) (Pp.value_to_string v');
	       `ChangeTo v'
	   | None -> `SkipChildren
	  )
      | _  -> `SkipChildren

    method visit_stmt = function
      | Ssa.Move(_,Val _, _) -> (* visit value will handle that properly *)
	  `DoChildren
      | Ssa.Move(v,e, a) -> (
	  match hash_replacement pos (vn v) with
	  | Some vl ->
	      changed := true;
	      dprintf "Replacing exp %s with %s" (Pp.ssa_exp_to_string e) (Pp.value_to_string vl);
	      `ChangeTo(Move(v, Val vl, a))
	  | None -> `DoChildren
	)
      | _ -> `DoChildren
  end
  in
  let somechange = ref false in
  let replace b cfg =
    let stmts = 
      List.mapi
	(fun i s ->
	   vis#set_pos (b,i);
	   Ssa_visitor.stmt_accept vis s
	)
	(C.get_stmts cfg b)
    in
    if !changed then (
      somechange := true;
      changed := false;
      C.set_stmts cfg b stmts)
    else cfg
  in
  pdebug "Doing replacement";
  let cfg = G.fold_vertex replace cfg cfg in
  (cfg, !somechange)



let aliased cfg =
  let (vn, _, _) = rpo cfg in
  fun x y -> match (x,y) with
  | (Int(i,_), Int(i',_)) ->
      Some(i = i')
  | (Lab x, Lab y) when x = y ->
      Some true
  | (Var x, Var y) when vn x = vn y ->
      Some true
  | _ -> 
      (* We could also check whether an lval was assigned a constant,
       * but running SCCVN.replace would get rid of any references to
       * such variables anyways. *)
      None


