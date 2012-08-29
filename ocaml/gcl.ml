(* Dijkstra's Guarded Command Language *)

open BatListFull
open Type
open Util
open Ast


module D = Debug.Make(struct let name = "GCL" and default=`Debug end)
open D

type var = Var.t
type exp = Ast.exp


(** A GCL expression.
    [Skip] does nothing.
    [Assign] assigns a value to an lvalue.
    [Seq(a,b)] evaluates [a] and then moves on to [b].
    [Choice(a,b)] 
    [Assert] goes on to the next expression in the sequence if it is true.
    [Assume] doesn't start when the condition is not true.
*)
type t =
  | Assume of exp
  | Assign of var * exp
  | Assert of exp
  | Choice of t * t
  | Seq of t * t
  | Skip 

(* let rec gcl_equal s1 s2 = *)
(*   let num = function *)
(*     | Assume _ -> 0 *)
(*     | Assign _ -> 1 *)
(*     | Assert _ -> 2 *)
(*     | Choice _ -> 3 *)
(*     | Seq _ -> 4 *)
(*     | Skip -> 5 *)
(*   in *)
(*   let getargs = function *)
(*     | Assume(e) -> [], e::[], [] *)
(*     | Assert(e) -> [], e::[], [] *)
(*     | Assign(v, e) -> v::[], e::[], [] *)
(*     | Choice(s1,s2) *)
(*     | Seq(s1,s2) -> [], [], s1::s2::[] *)
(*     | Skip -> [], [], [] *)
(*   in *)
(*   if num s1 <> num s2 then *)
(*     false *)
(*   else *)
(*     let l1,l2,l3 = getargs s1 *)
(*     and r1,r2,r3 = getargs s2 in *)
(*     let b1 = List.for_all2 (Var.equal) l1 r1 in *)
(*     let b2 = List.for_all2 quick_exp_eq l2 r2 in *)
(*     let b3 = List.for_all2 (==) l3 r3 in *)
(*     if b1 && b2 && b3 then *)
(*       true *)
(*     else *)
(*       b1 && *)
(*         List.for_all2 full_exp_eq l2 r2 && *)
(*         List.for_all2 gcl_equal l3 r3 *)

(** Convert a straightline trace into GCL.

    A straightline trace cannot have any CJmps, and any Jmps it might have must
    jump to the label following them. (Ie, any jump must be a no-op)
*)
let rec of_rev_straightline ?(acc=Skip) trace =
  let prepend g = function
    | Move(l,e,_) -> Seq(Assign(l,e), g)
    | CJmp _
    | Jmp _ ->
	invalid_arg "found Jmp in straightline GCL"
    | Comment _
    | Label _ ->
	g
    | Halt _ ->
      invalid_arg "Found halt in straightline code"
    | Special _ ->
	invalid_arg "Found special in straightline code"
    | Ast.Assert(e, _) -> Seq(Assert(e), g)
  in
  (* fold_left of reversed list, rather than fold_right, because
   * fold_right is not tail recursive *)
  try
    List.fold_left prepend acc trace
  with e ->
    Debug_snippets.print_ast trace;
    raise e

let of_straightline trace = of_rev_straightline(List.rev trace)



module CA = Cfg.AST

module RevCFG =
struct
  type t = CA.G.t
  module V = CA.G.V
  let iter_vertex = CA.G.iter_vertex
  let iter_succ = CA.G.iter_pred
  let in_degree = CA.G.out_degree
end

module RToposort = Graph.Topological.Make(RevCFG);;

(* intermediate type used by of_astcfg *)
type gclhelp =
  | CAssign of CA.G.V.t
  | CChoice of exp * gclhelp * gclhelp (* bb with cjmp, true  and false branches *)
  | Cunchoice of gclhelp * gclhelp (* unfinished choice *)
  | CSeq of gclhelp list

let rec cgcl_size = function
  | CAssign _ -> 1
  | CChoice(_, g1, g2)
  | Cunchoice(g1, g2) -> (cgcl_size g1) + (cgcl_size g2)
  | CSeq(l) -> List.fold_left (fun s n -> s + (cgcl_size n)) 0 l

let rec string_of_cgcl = function
  | CAssign(v) -> Cfg_ast.v2s v
  | CChoice(e, g1, g2) -> "CChoice("^Pp.ast_exp_to_string e^", "^string_of_cgcl g1^", "^string_of_cgcl g2^")"
  | Cunchoice(g1, g2) -> "Cunchoice("^string_of_cgcl g1^", "^string_of_cgcl g2^")"
  | CSeq(l) -> (List.fold_left (fun s g -> s^", "^string_of_cgcl g) "CSeq(" l)^")"

(* let rec cgcl_equal s1 s2 = *)
(*   let num = function *)
(*     | CAssign _ -> 0 *)
(*     | CChoice _ -> 1 *)
(*     | Cunchoice _ -> 2 *)
(*     | CSeq _ -> 3 *)
(*   in *)
(*   let getargs = function *)
(*     | CAssign(v) -> v::[], [], [] *)
(*     | CChoice(e,t1,t2) -> [], e::[], t1::t2::[] *)
(*     | Cunchoice(t1, t2) -> [], [], t1::t2::[] *)
(*     | CSeq(tlist) -> [], [], tlist *)
(*   in *)
(*   if num s1 <> num s2 then *)
(*     false *)
(*   else *)
(*     let l1,l2,l3 = getargs s1 *)
(*     and r1,r2,r3 = getargs s2 in *)
(*     let b1 = List.for_all2 (CA.G.V.equal) l1 r1 in *)
(*     let b2 = List.for_all2 quick_exp_eq l2 r2 in *)
(*     let b3 = List.for_all2 (==) l3 r3 in *)
(*     if b1 && b2 && b3 then *)
(*       true *)
(*     else *)
(*       b1 && *)
(*         List.for_all2 full_exp_eq l2 r2 && *)
(*         List.for_all2 cgcl_equal l3 r3 *)

(** [gclhelp_of_cfg cfg entry_node exit_node] returns an intermediate form that
    is between CFGs and the GCL. [cfg] must be acyclic. *)
let gclhelp_of_astcfg ?entry ?exit cfg =
  let exit = match exit with
    | None -> CA.G.V.create Cfg.BB_Exit
    | Some x -> x
  and entry = match entry with
    | None -> CA.G.V.create Cfg.BB_Entry
    | Some x -> x
  in
  (* our latice is a list option of GCL expressions to be put in sequence *)
  let meet l1 l2 =
    (* dprintf "l1: %d l2: %d" (cgcl_size (CSeq l1)) (cgcl_size (CSeq l2)); *)
    (* let (su, g1, g2) = split_common_suffix ~eq:(=) l1 l2 in *)
    (* dprintf "l1: %s\nl2: %s" (string_of_cgcl (CSeq l1)) (string_of_cgcl (CSeq l2)); *)
    let (su, g1, g2) = split_common_suffix ~eq:(==) l1 l2 in
    (* dprintf "suffix: %s" (string_of_cgcl (CSeq su)); *)
    (* dprintf "Suffix length %d" (cgcl_size (CSeq su)); *)
    (* dprintf "%s <> %s" (string_of_cgcl (List.hd (List.rev g1))) (string_of_cgcl (List.hd (List.rev g2))); *)
    Cunchoice(CSeq g1, CSeq g2)  :: su
  in
  (* a skip in this context is a CSeq(CSeq [],..) and the like *)
  (* let rec remove_skips g = *)
  (*   match g with *)
  (*   | CAssign _ -> g *)
  (*   | CSeq [] -> g *)
  (*   | CSeq [x] -> x *)
  (*   | CSeq sl -> *)
  (*       (\* <@ is the composition operator from bap_util. This *)
  (*          recursively goes through sl and calls remove_skips on *)
  (*          each list item, along with the filtering. *\) *)
  (*       CSeq(list_filter_some *)
  (*              ((function CSeq[] -> None | x -> Some x) <@ remove_skips) sl ) *)
  (*   | CChoice(c,s1,s2) -> CChoice(c, remove_skips s1, remove_skips s2) *)
  (*   | Cunchoice(s1,s2) -> Cunchoice(remove_skips s1, remove_skips s2) *)
  (* in *)
  (*   (\* finds first assignment *\) *)
  (* let rec find_first s = *)
  (*   match remove_skips s with *)
  (*   | CAssign b -> b *)
  (*   | CSeq(h::_) -> find_first h *)
  (*   | CSeq [] -> failwith "shouldn't ever get here tnh99btcn" *)
  (*   | CChoice _ -> failwith "shouldn't ever get here tnh9rh203" *)
  (*   | Cunchoice _ -> failwith "shouldn't ever get here tnh982h9o" *)
  (* in *)
  (* finds first assignment *)
  let find_first s =
    let rec find_first_h s =
      (* dprintf "skip town"; *)
      (* let s = remove_skips s in *)
      (* dprintf "done skip town"; *)
      match s with
      | CAssign b -> Some b
      | CSeq l -> Util.list_find_option find_first_h l
      | CChoice _ -> failwith "shouldn't ever get here tnh9rh203"
      | Cunchoice _ -> failwith "shouldn't ever get here tnh982h9o"
    in
    BatOption.get (find_first_h s)
  in
    (* find cjmp in a block. assumes it is the last statement in the
       block *)
  let rec find_cjmp = function
    | [] -> None
    | stmts ->
	match list_last stmts with
	| CJmp(c,t,f,_) -> Some(c,t,f)
	| s -> None
  in
  let find_target e =
    match lab_of_exp e with
    | Some l -> CA.find_label cfg l
    | _ -> failwith ("indirect jump not supported yet: "^(Pp.ast_exp_to_string e))
  in
  let b_to_string b = Cfg.bbid_to_string (CA.G.V.label b) in
  (* transfer function *)
  let f_t n = function
    | Cunchoice(bb1, bb2)::rest as exp ->
	(match find_cjmp (CA.get_stmts cfg n) with
	 | Some(cond,tt,ft) ->
	     let (bbt,bbf) =
	       match (find_first (CSeq(bb1::rest)), find_first (CSeq(bb2::rest)),
	              find_target tt, find_target ft) with
		 (b1,b2,bt,bf) when b1 = bt && b2 = bf -> (bb1,bb2)
	       | (b1,b2,bt,bf) when b2 = bt && b1 = bf -> (bb2,bb1)
	       | (b1,b2,bt,bf) ->
		   failwith(Printf.sprintf "choice seems to not correspond to cjmp %s %s %s %s %s %s %s %s at %s. %s"
			      (b_to_string b1) (b_to_string b2)
			      (b_to_string bt) (b_to_string bf)
                              (Pp.ast_exp_to_string tt) (Cfg_ast.v2s (find_target tt))
                              (Pp.ast_exp_to_string ft) (Cfg_ast.v2s (find_target ft))
			      (b_to_string n)
                              (Pp.ast_stmt_to_string (List.hd (List.rev (CA.get_stmts cfg n)))))
	     in
	     CAssign n::CChoice(cond, bbt, bbf)::rest
	 | None -> (* No CJmp found *)
	     dprintf "Warning: CJmp expected but not found at end of %s." (b_to_string n);
	     CAssign n::exp
	)
    | exp -> CAssign n::exp
  in
  let module BH = Hashtbl.Make(CA.G.V) in
  let h = BH.create (CA.G.nb_vertex cfg) in
  let get b =
    try BH.find h b
    with Not_found -> failwith("no GCL at "^b_to_string b)
  in
  (*BH.add h exit []; *)
  let compute_at b =
    let last_gcl = match CA.G.succ cfg b with
      | [p] -> get p
      | [x;y] ->
        (* let rx = Reachable.AST.reachable cfg x in *)
        (* let ry = Reachable.AST.reachable cfg y in *)
        (* let r = Util.list_intersection rx ry in *)
        (* dprintf "reachable intersection = %s" (String.concat " " (List.map Cfg_ast.v2s r)); *)
        meet (get x) (get y)
      | s when CA.G.V.equal b exit -> assert(s=[]); []
      | s when CA.G.V.label b = Cfg.BB_Error -> assert(s=[]); []
      | _ -> failwith("indirect jmp unsupported. "^b_to_string b^" had too many successors")
    in
    let gcl = f_t b last_gcl in
    BH.add h b gcl
  in
  RToposort.iter compute_at cfg;
  (CSeq (get entry))

(** [of_cfg cfg entry_node exit_node] will compute a function from entry node to 
    gcl between the entry node and the exit node. [cfg] must be acyclic. *)
let of_astcfg ?entry ?exit cfg =
  let cgcl_to_gcl s =
    (* k is a continuation *)
    let rec c s k = match s with
      | CChoice(cond, e1, e2) ->
        c e1 (fun ce1 ->
          c e2 (fun ce2 ->
            k (Choice(Seq(Assume cond, ce1),
                      Seq(Assume(exp_not cond), ce2)))))
      | Cunchoice(e1, e2) ->
        pwarn "generating an unguarded choice";
        c e1 (fun ce1 ->
          c e2 (fun ce2 ->
            k (Choice(ce1, ce2))))
      | CSeq [] ->
        k Skip
      | CSeq(e::[]) ->
      (* This isn't really a Seq at all! *)
        c e (fun ce -> k ce)
      | CSeq(e::es) ->
        (* dprintf "l: %d" (List.length (e::es)); *)
        c e (fun ce -> c (CSeq es) (fun ces -> k (Seq(ce, ces))))
      | CAssign b ->
        let bb_s = CA.get_stmts cfg b in
        let e = match List.rev bb_s with
          | [] -> Skip
          | (Jmp _ | CJmp _ | Halt _)::rest -> of_rev_straightline rest
          | _ -> of_straightline bb_s
        in
        k e
    in
    c s Util.id
  in
  cgcl_to_gcl (gclhelp_of_astcfg ?entry ?exit cfg)

let of_ast p =
  of_astcfg (Prune_unreachable.prune_unreachable_ast (Cfg_ast.of_prog p))


let rec remove_skips = function
  | Assume _
  | Assert _
  | Assign _
  | Skip as gcl -> gcl
  | Choice(g1,g2) -> (
      match (remove_skips g1 , remove_skips g2) with
      | (Skip, Skip) -> Skip
      | (a, b) -> Choice(a,b)
    )
  | Seq(g1,g2) -> (
      match (remove_skips g1 , remove_skips g2) with
      | Skip, Skip -> Skip
      | Skip, x -> x
      | x, Skip -> x
      | x, y -> Seq(x,y)
    )


module C = Cfg.SSA

let passified_of_ssa ?entry ?exit cfg =
  let ast = Cfg_ssa.to_astcfg ~dsa:true cfg in
  let convert = function
    | Some v -> Some(CA.find_vertex ast (C.G.V.label v))
    | None -> None
  in
  let entry = convert entry and exit = convert exit in
  let gcl = of_astcfg ?entry ?exit ast in
  let vars = ref [] in
  let rec convert_gcl g = 
    match g with
    | Assign(v,e) ->
	vars := v :: !vars;
	Assume(exp_eq (Var v) e)
    | Choice(a,b) ->
	Choice(convert_gcl a, convert_gcl b)
    | Seq(a,b) ->
	Seq(convert_gcl a, convert_gcl b)
    | Assume _ | Assert _ | Skip ->
	g
  in
  let pgcl = convert_gcl gcl in
  (pgcl, list_unique !vars)


let passified_of_astcfg ?entry ?exit cfg =
  let {Cfg_ssa.cfg=ssa; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let convert = function
    | Some v -> Some(C.find_vertex ssa (CA.G.V.label v))
    | None -> None
  in
  let entry = convert entry and exit = convert exit in
  let (g,v) = passified_of_ssa ?entry ?exit ssa in
  (g,v,tossa)
