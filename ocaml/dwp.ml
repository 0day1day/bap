open Ast
open Ast_convenience
module D = Debug.Make(struct let name = "Dwp" and default=`NoDebug end)
open D
open Gcl
open Type
module VM = Var.VarMap

module SymbolicMap = Symbeval.SymbolicSlowMap

let unwrap_symb = function
  | Symbeval.Symbolic e -> e
  | Symbeval.ConcreteMem(m,v) -> Symbeval.symb_to_exp (Symbeval.conc2symb m v)

module type Delta =
sig
  type t
  val create : unit -> t
  val merge : t -> t -> t * (var * Symbeval.varval option * Symbeval.varval option) list
  (** Take the intersection of two deltas. When there are conflicting
      bindings for a variable, there will be no binding in the final
      delta for that variable. *)
  (* val merge_super : t -> t -> exp -> exp -> Type.formula_mode -> t * exp * exp *)
  (* (\** [merge_super d1 d2 pi1 pi2] returns a merged [d], and modified *)
  (*     [pi1] and [pi2]. When there are conflicting bindings for a *)
  (*     variable, there will be no binding in the final delta for that *)
  (*     variable, and the binding will be added to that branch's *)
  (*     formula. *\) *)
  val set : t -> var -> Symbeval.varval -> t
  (** Setter method *)
  val get : t -> var -> Symbeval.varval
  (** Getter method.  Raises [Not_found] exception if variable is
      not set. *)
  val set_exp : t -> var -> exp -> t
  val get_exp : t -> var -> exp
  val simplify : t -> exp -> Symbeval.varval
  (** [simplify d e] simplifies exp in context d. *)
end

module VMDelta =
struct
  type t = Symbeval.varval VM.t
  let create () =
    VM.empty
  let merge (d1:t) (d2:t) =
    let o = ref [] in
    let f =
      VM.fold (fun var value newdelta ->
        try
          let newvalue = VM.find var newdelta in
          if value = newvalue then
          (* Newdelta already has the same value! We can just return newdelta as
             is. *)
            newdelta
          else (
          (* Newdelta has a CONFLICTING assignment.  We need to remove it. *)
            dprintf "CONFLICT: Removing %s" (Pp.var_to_string var);
            o := (var, Some value, Some newvalue) :: !o;
            VM.remove var newdelta)
        with Not_found ->
          (* Conflict: Var only assigned in one branch. Don't add it. *)
          dprintf "MISSING: Removing %s" (Pp.var_to_string var);
          o := (var, Some value, None) :: !o;
          newdelta
      ) d1 d2
    in
    (* f has all the correct bindings in d1.  Now we need to remove
       bindings that are only in d2. *)
    let f = VM.fold (fun var value newdelta ->
      if VM.mem var d1 then
        (* This is in d1 and d2, we're okay *)
        newdelta
      else (
        (* In d2 but not d1, remove *)
        o := (var, None, Some value) :: !o;
        VM.remove var newdelta)
    ) f f
    in
    f, !o
  (* let merge_super d1 d2 pi1 pi2 mode = *)
  (*   let f, pi1, pi2 = *)
  (*     VM.fold (fun var value (newdelta, pi1, pi2) -> *)
  (*       try *)
  (*         let newvalue = VM.find var newdelta in *)
  (*         if value = newvalue then *)
  (*         (\* Newdelta already has the same value! We can just return newdelta as *)
  (*            is. *\) *)
  (*           newdelta, pi1, pi2 *)
  (*         else ( *)
  (*           (\* Newdelta has a CONFLICTING assignment.  We need to remove *)
  (*              it, and add the assignments to pi1 and pi2. *\) *)
  (*           dprintf "CONFLICT: Removing %s" (Pp.var_to_string var); *)
  (*           let pi1 = Assign.add_passive_value pi1 var value mode in *)
  (*           let pi2 = Assign.add_passive_value pi2 var newvalue mode in *)
  (*           VM.remove var newdelta, pi1, pi2) *)
  (*       with Not_found -> *)
  (*         (\* Conflict: Var only assigned in d1. Add it to pi1. *\) *)
  (*         dprintf "MISSING: Removing %s" (Pp.var_to_string var); *)
  (*         let pi1 = Assign.add_passive_value pi1 var value mode in *)
  (*         newdelta, pi1, pi2 *)
  (*     ) d1 (d2, pi1, pi2) *)
  (*   in *)
  (*   (\* f has all the correct bindings in d1.  Now we need to remove *)
  (*      bindings that are only in d2. *\) *)
  (*   VM.fold (fun var value (newdelta, pi1, pi2) -> *)
  (*     if VM.mem var d1 then *)
  (*       (\* This is in d1 and d2, we're okay *\) *)
  (*       newdelta, pi1, pi2 *)
  (*     else *)
  (*       (\* In d2 but not d1, add to pi2. *\) *)
  (*       let newvalue = VM.find var d2 in *)
  (*       let pi2 = Assign.add_passive_value pi2 var newvalue mode in *)
  (*       VM.remove var newdelta, pi1, pi2 *)
  (*   ) f (f, pi1, pi2) *)
  let set h v e =
    dprintf "Setting %s to %s" (Pp.var_to_string v) (Symbeval.symb_to_string e);
    VM.add v e h
  let get h v =
    VM.find v h
  let get_exp h v =
    unwrap_symb (get h v)
  let set_exp h v e =
    set h v (Symbeval.Symbolic e)
  let simplify d e =
    (* Reduce to constant if possible *)
    SymbolicMap.eval_expr d e
end

(* This helps keeps ms syntactically equal to true *)
let or_simp = function
  | BinOp(OR, e1, e2) when e1 === exp_true || e2 === exp_true -> exp_true
  | BinOp(OR, e1, e2) when e1 === exp_false -> e2
  | BinOp(OR, e1, e2) when e2 === exp_false -> e1
  | BinOp(AND, e1, e2) when e1 === exp_true -> e2
  | BinOp(AND, e1, e2) when e2 === exp_true -> e1
  | e -> e

let rec dwp ?(simp=or_simp) ?(k=1) ?assign_mode p =
  let dwp = dwp ~simp ~k ?assign_mode in match p with
  | Assign (v,e) when assign_mode <> None ->
    (match assign_mode with
    | Some Sat -> dwp (Assert (exp_eq (Var v) e))
    | Some Validity -> dwp (Assume (exp_eq (Var v) e))
    | Some Foralls -> failwith "dwp: Foralls not implemented"
    | None -> failwith "dwp: impossible")
  | Assign _ -> failwith "dwp requires an assignment free program"
  | Assert e -> [], exp_true, exp_not e
  | Assume e -> [], e, exp_false
  | Choice (s1, s2) ->
    let v1, ms1, af1 = dwp s1 in
    let v2, ms2, af2 = dwp s2 in
    v1@v2, simp (exp_or ms1 ms2), simp (exp_or af1 af2)
  | Seq (s1, s2) ->
    let v1, ms1, af1 = dwp s1 in
    let v2, ms2, af2 = dwp s2 in
    let v = [] in
    let (v,ms1) = Wp.variableify ~name:"eddwp_seq_ms1" k v ms1 in
    let (v,af1) = Wp.variableify ~name:"eddwp_seq_af1" k v af1 in
    v@v1@v2, simp (exp_and ms1 (simp (exp_or af1 ms2))), simp (exp_and ms1 (simp (exp_or af1 af2)))
  | Skip -> [], exp_true, exp_false

(* Ed's DWP formulation + concrete evaluation.

   XXX: Fix simplification
 *)
module Make(D:Delta) = struct
  let dwp_conc ?(simp=or_simp) ?(k=1) ?(cf=true) (mode:formula_mode) (p:Gcl.t) =
    let eval delta e = if cf
      then D.simplify delta e
      else Symbeval.Symbolic e
    in
  (* let eval_exp delta e = unwrap_symb (eval delta e) in *)
    let punt delta s = let v, ms, af = dwp ~simp ~k s
                       in delta, v, ms, af
    in
    let rec dwpconc delta = function
      | Gcl.Assign (v, e) as s ->
        let value = eval delta e in
        if Symbeval.is_concrete_mem_or_scalar value then
          D.set delta v value, [], exp_true, exp_false
        else punt delta s
      | Gcl.Assume e as s -> punt delta s
      | Gcl.Assert e as s -> punt delta s
      | Gcl.Choice (s1, s2) ->
        let delta1, v1, ms1, af1 = dwpconc delta s1 in
        let delta2, v2, ms2, af2 = dwpconc delta s2 in
        let deltamerge, conflicts = D.merge delta1 delta2 in

        (* Merging gives us a list of variable conflicts in the two
           branches.  We can handle these by adding assignment
           statements to the end of each branch, and then using the
           Seq rule. (TODO: Prove this in Isabelle.)  However, we
           already have a lot of formula pieces precomputed, so
           instead of recursing and wasting all that work, we will
           include a version of the sequence rule here that reuses
           ms1, af1, etc.  *)

        let s1conflicts = BatList.filter_map (fun (v,x,_) ->
          match x with Some x -> Some(v,x) | None -> None) conflicts in
        let s2conflicts = BatList.filter_map (fun (v,_,x) ->
          match x with Some x -> Some(v,x) | None -> None) conflicts in
        let add_assign (ms,af) (v,e) =
          let _, ms2, af2 = dwp ~simp ~k ~assign_mode:mode (Assign (v, unwrap_symb e)) in
          let ms = exp_and ms (exp_or af ms2) in
          let af = exp_and ms (exp_or af af2) in (ms, af)
        in
        let (ms1, af1) = List.fold_left add_assign (ms1, af1) s1conflicts in
        let (ms2, af2) = List.fold_left add_assign (ms2, af2) s2conflicts in

        deltamerge, v1@v2, simp (exp_or ms1 ms2), simp (exp_or af1 af2)
      | Gcl.Seq (s1, s2) ->
        let delta1, v1, ms1, af1 = dwpconc delta s1 in
        let delta2, v2, ms2, af2 = dwpconc delta1 s2 in
        delta2, v1@v2, simp (exp_and ms1 (simp (exp_or af1 ms2))), simp (exp_and ms1 (simp (exp_or af1 af2)))
      | Gcl.Skip as s -> punt delta s
    in
    dwpconc (D.create ()) p
end

(* Ed's DWP formulation.  If there are no Assumes, dwpms will always
   be true, and dwp degenerates to efficient (merging) fse. *)
let eddwp ?(simp=or_simp) ?(k=1) (mode:formula_mode) (p:Gcl.t) q =
  (*
    Returns (v, dwpms P, dwpaf P).

    Note: dwpms P = Not (wp P true) \/ Not (wlp P false)
      and dwpaf P = Not (wp P true) *)
  dprintf "go go go";
  let (v,ms,af) = dwp ~simp ~k p in
  dprintf "yay";
  let vo = Wp.assignments_to_exp v in
  match mode with
  | Sat ->
    exp_and vo (exp_implies ms (exp_and (exp_not af) q))
  | Validity ->
    exp_implies vo (exp_implies ms (exp_and (exp_not af) q))
  | Foralls ->
    failwith "Foralls not supported yet"

(* Ed's DWP formulation.  If there are no Assumes, dwpms will always
   be true, and dwp degenerates to efficient (merging) fse. *)
let eddwp_conc ?(simp=or_simp) ?(k=1) (mode:formula_mode) (p:Gcl.t) q =
  (*
    Returns (v, dwpms P, dwpaf P).

    Note: dwpms P = Not (wp P true) \/ Not (wlp P false)
      and dwpaf P = Not (wp P true) *)
  let module DWPCONC = Make(VMDelta) in
  let (_,v,ms,af) = DWPCONC.dwp_conc ~simp ~k mode p in
  let vo = Wp.assignments_to_exp v in
  match mode with
  | Sat ->
    exp_and vo (exp_implies ms (exp_and (exp_not af) q))
  | Validity ->
    exp_implies vo (exp_implies ms (exp_and (exp_not af) q))
  | Foralls ->
    failwith "Foralls not supported yet"
