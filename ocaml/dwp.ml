open Ast
open Ast_convenience
module D = Debug.Make(struct let name = "Dwp" and default=`NoDebug end)
open D
open Gcl
open Symbeval
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
  | BinOp(OR, e1, UnOp(NOT, e2))
  | BinOp(OR, UnOp(NOT, e1), e2) when e1 === e2 -> exp_true
  | BinOp(AND, e1, e2) when e1 === exp_true -> e2
  | BinOp(AND, e1, e2) when e2 === exp_true -> e1
  | BinOp(AND, e1, e2) when e1 === exp_false || e2 === exp_false -> exp_false
  | BinOp(AND, e1, UnOp(NOT, e2))
  | BinOp(AND, UnOp(NOT, e1), e2) when e1 === e2 -> exp_false
  | e -> e

(* Dwp base implementation.  Returns a tuple consisting of variable
   bindings, de-duplicated ms, de-duplicated af, duplicated ms,
   duplicated af.  We need to keep both duplicated and de-duplicated
   versions of expressions for simplification.  For instance, we
   cannot infer e \/ \lnot e = true if if is written as v=e /\ (v \/
   \lnot e).  Yet, simplifications are very important because for Sat
   queries MS should simplify to true!

   Memory is not a concern for the duplicated expressions because of
   memory sharing.
*)
let rec dwp ?(simp=or_simp) ?(k=1) ?assign_mode p =
  (* Use the simplified duplicate expression if it is a constant,
     otherwise use the duplicated one. *)
  let choose_best e edup =
    match edup with
    | Int _ -> edup
    | _ -> e
  in
  let dwp = dwp ~simp ~k ?assign_mode in match p with
    | Assign (v,e) when assign_mode <> None ->
      (match assign_mode with
      | Some Sat -> dwp (Assert (exp_eq (Var v) e))
      | Some Validity -> dwp (Assume (exp_eq (Var v) e))
      | Some Foralls -> failwith "dwp: Foralls not implemented"
      | None -> failwith "dwp: impossible")
    | Assign _ -> failwith "dwp requires an assignment free program"
    | Assert e -> [], exp_true, exp_not e, exp_true, exp_not e
    | Assume e -> [], e, exp_false, e, exp_false
    | Choice (s1, s2) ->
      let v1, ms1, af1, msdup1, afdup1 = dwp s1 in
      let v2, ms2, af2, msdup2, afdup2 = dwp s2 in

      let ms = simp (exp_or ms1 ms2) in
      let msdup = simp (exp_or msdup1 msdup2) in
      let af = simp (exp_or af1 af2) in
      let afdup = simp (exp_or afdup1 afdup2) in

      v1@v2, choose_best ms msdup, choose_best af afdup, msdup, afdup
    | Seq (s1, s2) as _s ->
      let v1, ms1, af1, msdup1, afdup1 = dwp s1 in
      let v2, ms2, af2, msdup2, afdup2 = dwp s2 in
      let v = [] in
      let (v,ms1) = Wp.variableify ~name:"eddwp_seq_ms1" k v ms1 in
      let (v,af1) = Wp.variableify ~name:"eddwp_seq_af1" k v af1 in

      let ms = simp (exp_and ms1 (simp (exp_or af1 ms2))) in
      let msdup = simp (exp_and msdup1 (simp (exp_or afdup1 msdup2))) in
      let af = simp (exp_and ms1 (simp (exp_or af1 af2))) in
      let afdup = simp (exp_and msdup1 (simp (exp_or afdup1 afdup2))) in

      v@v1@v2, choose_best ms msdup, choose_best af afdup, msdup, afdup
  | Skip -> [], exp_true, exp_false, exp_true, exp_false

module Make(D:Delta) = struct

(* Ed's DWP formulation + concrete evaluation. *)
let eddwp_conc ?(simp=or_simp) ?(k=1) ?(cf=true) (mode:formula_mode) (p:Gcl.t) q =
  (*
    Returns (v, dwpms P, dwpaf P).

    Note: dwpms P = Not (wp P true) \/ Not (wlp P false)
      and dwpaf P = Not (wp P true) *)
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  let punt delta s = let v, ms, af, _, _ = dwp ~simp ~k ~assign_mode:mode s
                     in delta, v, ms, af
  in
  let rec dwpconc delta = function
    | Gcl.Assign (v, e) as s ->
      let value = eval delta e in
      if Symbeval.is_concrete_mem_or_scalar value then
        D.set delta v value, [], exp_true, exp_false
      else punt delta s
    | Gcl.Assume e as s ->
      let value = eval delta e in
      if value = Symbolic exp_true then punt delta Skip
      else if value = Symbolic exp_false then delta, [], exp_false, exp_false
      else punt delta s
    | Gcl.Assert e as s ->
      let value = eval delta e in
      if value = Symbolic exp_true then punt delta Skip
      else if value = Symbolic exp_false then delta, [], exp_true, exp_true
      else punt delta s
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
        let _, ms2, af2, _, _ = dwp ~simp ~k ~assign_mode:mode (Assign (v, unwrap_symb e)) in
        let ms = simp (exp_and ms (simp (exp_or af ms2))) in
        let af = simp (exp_and ms (simp (exp_or af af2))) in (ms, af)
      in
      let (ms1, af1) = List.fold_left add_assign (ms1, af1) s1conflicts in
      let (ms2, af2) = List.fold_left add_assign (ms2, af2) s2conflicts in

      deltamerge, v1@v2, simp (exp_or ms1 ms2), simp (exp_or af1 af2)
    | Gcl.Seq (s1, s2) as _s ->
      let delta1, v1, ms1, af1 = dwpconc delta s1 in
      if ms1 = exp_false then delta1, v1, exp_false, exp_false
      else if af1 = exp_true then delta1, v1, ms1, ms1
      else
        let delta2, v2, ms2, af2 = dwpconc delta1 s2 in
        let v = [] in
        let (v,ms1) = Wp.variableify ~name:"eddwp_seq_ms1" k v ms1 in
        let (v,af1) = Wp.variableify ~name:"eddwp_seq_af1" k v af1 in
        delta2, v@v1@v2, simp (exp_and ms1 (simp (exp_or af1 ms2))), simp (exp_and ms1 (simp (exp_or af1 af2)))
    | Gcl.Skip as s -> punt delta s
  in
  let (delta,v,ms,af) = dwpconc (D.create ()) p in
  let q' =
    let value = eval delta q in
    unwrap_symb value
  in
  let vo = Wp.assignments_to_exp v in
  match mode with
  | Sat ->
    exp_and vo (exp_implies ms (exp_and (exp_not af) q'))
  | Validity ->
    exp_implies vo (exp_implies ms (exp_and (exp_not af) q'))
  | Foralls ->
    failwith "Foralls not supported yet"

(* Ed's DWP formulation + concrete evaluation + lazy merging.

   This function concrete evaluates the entire program eagerly, but
   produces lazy expressions for the formulas.  We do this so that we
   know what variables need to be placed in the formula at merge
   points.  For instance, in the program [Choice(Assign (x, 5), Skip)]
   we do not need to put x in the formula, because there is no
   reference to [x] in the formula. However, [Seq(Choice(Assign (x,
   5), Skip), Assert (x == 5))] would need to put [x] in the formula.
   The difficulty is that DWP is going forward, but we don't know if a
   variable is used until later in the program.  Thus the lazy
   expressions.
*)
let eddwp_lazyconc ?(simp=or_simp) ?(k=1) ?(cf=true) (mode:formula_mode) (p:Gcl.t) q =
  (*
    Returns (v, dwpms P, dwpaf P).

    Note: dwpms P = Not (wp P true) \/ Not (wlp P false)
    and dwpaf P = Not (wp P true) *)
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  let h = VH.create 1000 in
  let needed v =
    try VH.find h v
    with Not_found -> false
  (* Mark v as being needed in the formula *)
  and used v =
    VH.replace h v true
  in
  let used_vars_in e =
    let v = object
      inherit Ast_visitor.nop
      method visit_rvar v =
        used v;
        DoChildren
    end
    in
    ignore(Ast_visitor.exp_accept v e)
  in
  let punt delta s = let v, ms, af, _, _ = dwp ~simp ~k ~assign_mode:mode s
                     in
                     let e = match s with
                     | Gcl.Assign (_, e) -> Some e
                     | Gcl.Assume e -> Some e
                     | Gcl.Assert e -> Some e
                     | Gcl.Skip -> None
                     | _ -> failwith "Not allowed to punt for complex statements"
                     in
                     BatOption.may used_vars_in e;
                     delta, v, ms, af
  in
  let punt_internal delta s = let _, v, ms, af = punt delta s in
                              (v, ms, af)
  in
  let punt_external delta s = let delta, v, ms, af = punt delta s in
                              delta, lazy (v,  ms, af)
  in
  let rec dwpconc delta = function
    | Gcl.Assign (v, e) as s ->
      let value = eval delta e in
      if Symbeval.is_concrete_mem_or_scalar value then
        let delta = D.set delta v value in
        delta, lazy (
          if needed v
          (* If needed in the formula, insert the concrete value to the formula.
             XXX: Should we do this for concrete memories? *)
          then punt_internal delta (Gcl.Assign (v, unwrap_symb value))
          (* If the value is not needed in the formula, act like a Skip *)
          else punt_internal delta Gcl.Skip
        ) else punt_external delta s
    | Gcl.Assume e as s ->
      let value = eval delta e in
      if value = Symbolic exp_true then punt_external delta Gcl.Skip
      else if value = Symbolic exp_false then delta, lazy([], exp_false, exp_false)
      else punt_external delta s
    | Gcl.Assert e as s ->
      let value = eval delta e in
      if value = Symbolic exp_true then punt_external delta Skip
      else if value = Symbolic exp_false then delta, lazy([], exp_true, exp_true)
      else punt_external delta s
    | Gcl.Choice (s1, s2) ->
      let delta1, lazy1 = dwpconc delta s1 in
      let delta2, lazy2 = dwpconc delta s2 in
      let deltamerge, conflicts = D.merge delta1 delta2 in

        (* Merging gives us a list of variable conflicts in the two
           branches.  We can handle these by adding assignment
           statements to the end of each branch, and then using the
           Seq rule. (TODO: Prove this in Isabelle.)  However, we
           already have a lot of formula pieces precomputed, so
           instead of recursing and wasting all that work, we will
           include a version of the sequence rule here that reuses
           ms1, af1, etc.  *)

      let () = List.iter (fun (v,_,_) -> used v) conflicts in

      (* let s1conflicts = BatList.filter_map (fun (v,x,_) -> *)
      (*   match x with Some x -> Some(v,x) | None -> None) conflicts in *)
      (* let s2conflicts = BatList.filter_map (fun (v,_,x) -> *)
      (*   match x with Some x -> Some(v,x) | None -> None) conflicts in *)
      (* let add_assign (ms,af) (v,e) = *)
      (*   let _, ms2, af2 = dwp ~simp ~k ~assign_mode:mode (Assign (v, unwrap_symb e)) in *)
      (*   let ms = simp (exp_and ms (simp (exp_or af ms2))) in *)
      (*   let af = simp (exp_and ms (simp (exp_or af af2))) in (ms, af) *)
      (* in *)
      (* let (ms1, af1) = List.fold_left add_assign (ms1, af1) s1conflicts in *)
      (* let (ms2, af2) = List.fold_left add_assign (ms2, af2) s2conflicts in *)

      deltamerge, lazy (
        let v1, ms1, af1 = Lazy.force lazy1 in
        let v2, ms2, af2 = Lazy.force lazy2 in
        v1@v2, simp (exp_or ms1 ms2), simp (exp_or af1 af2))
    | Gcl.Seq (s1, s2) as _s ->
      let delta1, lazy1 = dwpconc delta s1 in
      let delta2, lazy2 = dwpconc delta1 s2 in
      delta2, lazy (
        let v1, ms1, af1 = Lazy.force lazy1 in
        if ms1 = exp_false then v1, exp_false, exp_false
        else if af1 = exp_true then v1, ms1, ms1
        else
          let v2, ms2, af2 = Lazy.force lazy2 in
          let v = [] in
          let (v,ms1) = Wp.variableify ~name:"eddwp_seq_ms1" k v ms1 in
          let (v,af1) = Wp.variableify ~name:"eddwp_seq_af1" k v af1 in
          v@v1@v2, simp (exp_and ms1 (simp (exp_or af1 ms2))), simp (exp_and ms1 (simp (exp_or af1 af2))))
    | Gcl.Skip as s -> punt_external delta s
  in
  let (delta, lazyr) = dwpconc (D.create ()) p in
  let v, ms, af = Lazy.force lazyr in
  let q' =
    let value = eval delta q in
    unwrap_symb value
  in
  let vo = Wp.assignments_to_exp v in
  match mode with
  | Sat ->
    exp_and vo (exp_implies ms (exp_and (exp_not af) q'))
  | Validity ->
    exp_implies vo (exp_implies ms (exp_and (exp_not af) q'))
  | Foralls ->
    failwith "Foralls not supported yet"
end

(* Ed's DWP formulation.  If there are no Assumes, dwpms will always
   be true, and dwp degenerates to efficient (merging) fse. *)
let eddwp ?(simp=or_simp) ?(k=1) (mode:formula_mode) (p:Gcl.t) q =
  (*
    Returns (v, dwpms P, dwpaf P).

    Note: dwpms P = Not (wp P true) \/ Not (wlp P false)
      and dwpaf P = Not (wp P true) *)
  let (v,ms,af,_,_) = dwp ~simp ~k p in
  let vo = Wp.assignments_to_exp v in
  match mode with
  | Sat ->
    exp_and vo (exp_implies ms (exp_and (exp_not af) q))
  | Validity ->
    exp_implies vo (exp_implies ms (exp_and (exp_not af) q))
  | Foralls ->
    failwith "Foralls not supported yet"

let eddwp_conc ?simp ?k ?cf mode p q =
  let module DWPCONC = Make(VMDelta) in
  DWPCONC.eddwp_conc ?simp ?k ?cf mode p q

let eddwp_lazyconc ?simp ?k ?cf mode p q =
  let module DWPCONC = Make(VMDelta) in
  DWPCONC.eddwp_lazyconc ?simp ?k ?cf mode p q
