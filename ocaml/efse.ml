(** Implementation of [efse] algorithm from DWP paper. *)

open Ast
open Big_int_convenience
module CA = Cfg.AST
module D = Debug.Make(struct let name = "Efse" and default = `NoDebug end)
open D
open Type
module VM = Var.VarMap
type var = Ast.var
type exp = Ast.exp

type stmt = | Assign of (var * exp)
            | Assert of exp
            | Ite of (exp * prog * prog)
and prog = stmt list

type maybe = Valid | Sat | Unsat

module SymbolicMap = Symbeval.SymbolicSlowMap

let rec stmt_to_string = function
  | Assign(v,e) -> Printf.sprintf "%s = %s" (Pp.var_to_string v) (Pp.ast_exp_to_string e)
  | Assert e -> Printf.sprintf "Assert %s" (Pp.ast_exp_to_string e)
  | Ite(e, s1, s2) -> Printf.sprintf "If %s Then (%s) Else (%s)" (Pp.ast_exp_to_string e) (prog_to_string s1) (prog_to_string s2)
and prog_to_string = function
  | [] -> "/* Skip */"
  | x::[] -> stmt_to_string x
  | x::tl -> (stmt_to_string x)^"; "^(prog_to_string tl)

(* let stmt_to_string x = if not debug then "" else stmt_to_string x *)
(* let prog_to_string x = "" *)

let unwrap_symb = function
  | Symbeval.Symbolic e -> e
  | Symbeval.ConcreteMem(m,v) -> Symbeval.symb_to_exp (Symbeval.conc2symb m v)

module Assign =
struct

  let add_passive_assignment pi v e = function
    | Type.Foralls ->
      failwith "Foralls formula mode unsupported"
    | Type.Sat ->
      exp_and pi (exp_eq (Var v) e)
    | Type.Validity ->
      failwith "Validity is broken in efse"
      (* exp_implies (exp_eq (Var v) e) pi *)

  let add_passive_value pi v value =
    add_passive_assignment pi v (unwrap_symb value)
end

module ToEfse = struct
  let of_rev_straightline stmts =
    let rec f acc = function
      | [] -> acc
      | Ast.Move(v,e,_)::tl -> f (Assign(v,e)::acc) tl
      | Ast.Assert(e,_)::tl -> f (Assert(e)::acc) tl
      | Ast.Label _::tl
      | Ast.Comment _::tl -> f acc tl
      | s::_ -> failwith (Printf.sprintf "Found unexpected statement in straightline code: %s" (Pp.ast_stmt_to_string s))
    in
    f [] stmts

  let of_straightline stmts = of_rev_straightline (List.rev stmts)

  let of_astcfg ?entry ?exit cfg =
    let cgcl_to_fse s =
    (* k is a continuation *)
      let rec c s (k : prog -> prog) = match s with
        | Gcl.CChoice(cond, e1, e2) ->
          c e1 (fun ce1 ->
            c e2 (fun ce2 ->
              k [Ite(cond, ce1, ce2)]))
        | Gcl.Cunchoice(e1, e2) ->
          failwith "Unguarded choices not allowed"
        | Gcl.CSeq [] ->
          k []
        | Gcl.CSeq(e::es) ->
          (* dprintf "l: %d" (List.length (e::es)); *)
          c e (fun ce -> c (Gcl.CSeq es) (fun ces ->
            (* holy parenthesization bug! Grr ML why does @ have
               higher precedence than function application? *)
            k (ce@ces)))
        | Gcl.CAssign b ->
          let bb_s = Cfg.AST.get_stmts cfg b in
          let e = match List.rev bb_s with
            | [] -> []
            | (Ast.Jmp _ | Ast.CJmp _ | Ast.Halt _)::rest -> of_rev_straightline rest
            | _ -> of_straightline bb_s
          in
          k e
      in
      c s Util.id
    in
    if debug () then Pp.output_varnums := true;
    dprintf "Converting to gclhelp";
    let gcl = Gcl.gclhelp_of_astcfg ?entry ?exit cfg in
    (* If debug is off, dprintf will still evaluate it's arguments,
       which is why we insert a conditional. *)
    if debug () then dprintf "gcl: %s" (Gcl.gclhelp_to_string gcl);
    dprintf "Converting to fse";
    let fse = cgcl_to_fse (Gcl.gclhelp_of_astcfg ?entry ?exit cfg) in
    if debug () then dprintf "fse: %s" (prog_to_string fse);
    fse

  let passified_of_ssa ?entry ?exit cfg =
    let ast = Cfg_ssa.to_astcfg ~dsa:true cfg in
    let convert = function
      | Some v -> Some(CA.find_vertex ast (Cfg.SSA.G.V.label v))
      | None -> None
    in
    let entry = convert entry and exit = convert exit in
    of_astcfg ?entry ?exit ast

  let passified_of_astcfg ?entry ?exit cfg =
    let {Cfg_ssa.cfg=ssa; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
    let convert = function
      | Some v -> Some(Cfg.SSA.find_vertex ssa (CA.G.V.label v))
      | None -> None
    in
    let entry = convert entry and exit = convert exit in
    let g = passified_of_ssa ?entry ?exit ssa in
    (g,tossa)


end
include ToEfse


module type Delta =
sig
  type t
  val create : unit -> t
  val merge : t -> t -> t * var list
  (** Take the intersection of two deltas. When there are conflicting
      bindings for a variable, there will be no binding in the final
      delta for that variable. *)
  val merge_super : t -> t -> exp -> exp -> Type.formula_mode -> t * exp * exp
  (** [merge_super d1 d2 pi1 pi2] returns a merged [d], and modified
      [pi1] and [pi2]. When there are conflicting bindings for a
      variable, there will be no binding in the final delta for that
      variable, and the binding will be added to that branch's
      formula. *)
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
            o := var :: !o;
            VM.remove var newdelta)
        with Not_found ->
          (* Conflict: Var only assigned in one branch. Don't add it. *)
          dprintf "MISSING: Removing %s" (Pp.var_to_string var);
          o := var :: !o;
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
        o := var :: !o;
        VM.remove var newdelta)
    ) f f
    in
    f, !o
  let merge_super d1 d2 pi1 pi2 mode =
    let f, pi1, pi2 =
      VM.fold (fun var value (newdelta, pi1, pi2) ->
        try
          let newvalue = VM.find var newdelta in
          if value = newvalue then
          (* Newdelta already has the same value! We can just return newdelta as
             is. *)
            newdelta, pi1, pi2
          else (
            (* Newdelta has a CONFLICTING assignment.  We need to remove
               it, and add the assignments to pi1 and pi2. *)
            dprintf "CONFLICT: Removing %s" (Pp.var_to_string var);
            let pi1 = Assign.add_passive_value pi1 var value mode in
            let pi2 = Assign.add_passive_value pi2 var newvalue mode in
            VM.remove var newdelta, pi1, pi2)
        with Not_found ->
          (* Conflict: Var only assigned in d1. Add it to pi1. *)
          dprintf "MISSING: Removing %s" (Pp.var_to_string var);
          let pi1 = Assign.add_passive_value pi1 var value mode in
          newdelta, pi1, pi2
      ) d1 (d2, pi1, pi2)
    in
    (* f has all the correct bindings in d1.  Now we need to remove
       bindings that are only in d2. *)
    VM.fold (fun var value (newdelta, pi1, pi2) ->
      if VM.mem var d1 then
        (* This is in d1 and d2, we're okay *)
        newdelta, pi1, pi2
      else
        (* In d2 but not d1, add to pi2. *)
        let newvalue = VM.find var d2 in
        let pi2 = Assign.add_passive_value pi2 var newvalue mode in
        VM.remove var newdelta, pi1, pi2
    ) f (f, pi1, pi2)
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

module Make(D:Delta) =
struct

  (* Substitute any reference to a variable with it's value in
     delta.

     XXX: Support Let bindings.
  *)
  let sub_eval delta e =
    let v = object(self)
      inherit Ast_visitor.nop
      (* We can't use rvar because we need to return an exp. *)
      method visit_exp = function
        | Ast.Var v ->
          (try
          (* do NOT do children, because expressions are already
             evaluated. *)
            ChangeTo (D.get_exp delta v)
          with Not_found ->
            DoChildren)
        | _ -> DoChildren
    end in
    Ast_visitor.exp_accept v e

(** Inefficient fse algorithm for unpassified programs. *)
  let fse_unpass ?(cf=true) p post =
    let eval delta e = if cf
      then D.simplify delta e
      else Symbeval.Symbolic e
    in
    (* let eval delta e = if cf then ( *)
    (*   let x = D.simplify delta e in *)
    (*   if Symbeval.is_concrete_mem_or_scalar x then x *)
    (*   else Symbeval.Symbolic (sub_eval delta e) *)
    (* ) else Symbeval.Symbolic (sub_eval delta e) *)
    (* in *)
    let eval_exp delta e = unwrap_symb (eval delta e) in
    let rec fse_unpass delta pi = function
      | [] -> pi
      | Assign(v, e)::tl ->
        let value = eval delta e in
        let delta' = D.set delta v value in
        fse_unpass delta' pi tl
      | Assert(e)::tl ->
        let value = eval_exp delta e in
        let pi' = Ast.exp_and pi value in
        fse_unpass delta pi' tl
      | Ite(e, s1, s2)::tl ->
        let value_t = eval_exp delta e in
        let pi_t = Ast.exp_and pi value_t in
        let value_f = Ast.exp_not value_t in
        let pi_f = Ast.exp_and pi value_f in
        let fse_t = fse_unpass delta pi_t (s1@tl) in
        let fse_f = fse_unpass delta pi_f (s2@tl) in
        Ast.exp_or fse_t fse_f
    in
    (* For passified programs, pre=post, so we can set pi to post.
       But not here, since the program is not passified. *)
    let p = BatList.append p [Assert post] in
    fse_unpass (D.create ()) exp_true p

(** Inefficient fse algorithm for passified programs. *)
let fse_pass ?(cf=true) p post mode =
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  let eval_exp delta e = unwrap_symb (eval delta e) in
  let rec fse_pass delta pi = function
    | [] -> pi
    | Assign(v, e)::tl ->
      let value = eval delta e in
      let delta',pi' = if Symbeval.is_concrete_mem_or_scalar value then
        D.set delta v value (** Update for constants, no need to add to pi *),
          pi
        else delta, Assign.add_passive_value pi v value mode
      in
      fse_pass delta' pi' tl
    | Assert(e)::tl ->
      let value = eval_exp delta e in
      (match value with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi ->
        (* Assert false = false, pi \land false = false *)
        Ast.exp_false
      | Ast.Int(bi, Reg 1) when bi_is_one bi ->
        (* Assert true = true, pi \land true = pi *)
        fse_pass delta pi tl
      | _ ->
        let pi' = Ast.exp_and pi value in
        fse_pass delta pi' tl)
    | Ite(e, s1, s2)::tl ->
      let value_t = eval_exp delta e in
      (match value_t with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi ->
        fse_pass delta pi (s2@tl)
      | Ast.Int(bi, Reg 1) when bi_is_one bi ->
        fse_pass delta pi (s1@tl)
      | _ ->
        let pi_t = Ast.exp_and pi value_t in
        let value_f = Ast.exp_not value_t in
        let pi_f = Ast.exp_and pi value_f in
        let fse_t = fse_pass delta pi_t (s1@tl) in
        let fse_f = fse_pass delta pi_f (s2@tl) in
        Ast.exp_or fse_t fse_f)
  in
  let p = BatList.append p [Assert post] in
  fse_pass (D.create ()) exp_true p

(** Efficient fse algorithm for passified programs. *)
let efse ?(cf=true) p pi mode =
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  (* let eval delta e = if cf then ( *)
  (*   let x = D.simplify delta e in *)
  (*   if Symbeval.is_concrete_mem_or_scalar x then x *)
  (*   else Symbeval.Symbolic e *)
  (* ) else Symbeval.Symbolic e *)
  (* in *)
  let eval_exp delta e = unwrap_symb (eval delta e) in
  let rec efse delta pi = function
    | [] -> dprintf "empty"; delta,pi
    | Assign(v, e) as s::tl ->
      dprintf "assign";
      let value = eval delta e in
      dprintf "stmt: %s\nevaluated %s to %s, concrete = %b" (stmt_to_string s) (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (unwrap_symb value)) (Symbeval.is_concrete_mem_or_scalar value);
      let delta',pi' = match value with
        (* Note: Even concrete assignments need to be added to the
           formula.  This is because when Ite merged two contexts,
           conflicting concrete assignments will be expunged from the
           concrete context.  In this case, the assignment in the
           formula must be used.

           However, we need to be careful about concrete memories,
           because they aren't really constant size. So, we won't add
           the evaluated memory to the formula.  *)
        | Symbeval.Symbolic(Ast.Int _) when cf -> D.set delta v value, Assign.add_passive_value pi v value mode
        | Symbeval.ConcreteMem _ when cf -> D.set delta v value, Assign.add_passive_assignment pi v e mode
        | _ -> delta, Assign.add_passive_assignment pi v e mode
      in
      dprintf "done.";
      efse delta' pi' tl
    | Assert e::tl ->
      dprintf "assert";
      let value = eval_exp delta e in
      dprintf "Evaluated %s to %s" (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string value);
      (match value with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi && cf ->
        (* Assert false = false, pi \land false = false *)
        delta,Ast.exp_false
      | Ast.Int(bi, Reg 1) when bi_is_one bi && cf ->
        (* Assert true = true, pi \land true = pi *)
        efse delta pi tl
      | _ ->
        let pi' = Ast.exp_and pi value in
        efse delta pi' tl)
    | Ite(e, s1, s2)::tl ->
      dprintf "ite condition %s" (Pp.ast_exp_to_string e);
      let value_t = eval_exp delta e in
      (match value_t with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi && cf ->
        dprintf "true branch";
        efse delta pi (s2@tl)
      | Ast.Int(bi, Reg 1) when bi_is_one bi && cf ->
        dprintf "false branch";
        efse delta pi (s1@tl)
      | _ ->
        dprintf "symbolic branch";
        let delta1,pi_t = efse delta value_t s1 in
        let delta2,pi_f = efse delta (Ast.exp_not value_t) s2 in
        let mergedelta,_ = D.merge delta1 delta2 in
        let deltatl,pitl = efse mergedelta Ast.exp_true tl in
        deltatl, Ast.exp_and (Ast.exp_and pi (Ast.exp_or pi_t pi_f)) pitl)
  in
  let _,pi = efse (D.create ()) pi p in
  pi

(** Efficient fse algorithm for passified programs. 

    This implementation only adds concrete assignments to the formula when
    merging conflicts happen.
*)
let efse_merge1 ?(cf=true) p pi mode =
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  (* let eval delta e = if cf then ( *)
  (*   let x = D.simplify delta e in *)
  (*   if Symbeval.is_concrete_mem_or_scalar x then x *)
  (*   else Symbeval.Symbolic e *)
  (* ) else Symbeval.Symbolic e *)
  (* in *)
  let eval_exp delta e = unwrap_symb (eval delta e) in
  let rec efse delta pi = function
    | [] -> dprintf "empty"; delta,pi
    | Assign(v, e) as s::tl ->
      dprintf "assign";
      let value = eval delta e in
      dprintf "stmt: %s\nevaluated %s to %s, concrete = %b" (stmt_to_string s) (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (unwrap_symb value)) (Symbeval.is_concrete_mem_or_scalar value);
      let delta',pi' = match value with
        (* Note: Concrete assignments are not added to the formula
           here, but may be by merge_super.  Imagine if x=1 and x=0 occur
           on different branches of an Ite. *)
        | Symbeval.Symbolic(Ast.Int _) when cf -> D.set delta v value, (*Ast.exp_and pi (Ast.exp_eq (Ast.Var v) (unwrap_symb value))*) pi
        | Symbeval.ConcreteMem _ when cf -> D.set delta v value, (*Ast.exp_and pi (Ast.exp_eq (Ast.Var v) e)*) pi
        | _ -> delta, Assign.add_passive_assignment pi v e mode
      in
      dprintf "done.";
      efse delta' pi' tl
    | Assert e::tl ->
      dprintf "assert";
      let value = eval_exp delta e in
      dprintf "Evaluated %s to %s" (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string value);
      (match value with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi && cf ->
        (* Assert false = false, pi \land false = false *)
        delta,Ast.exp_false
      | Ast.Int(bi, Reg 1) when bi_is_one bi && cf ->
        (* Assert true = true, pi \land true = pi *)
        efse delta pi tl
      | _ ->
        let pi' = Ast.exp_and pi value in
        efse delta pi' tl)
    | Ite(e, s1, s2)::tl ->
      dprintf "ite condition %s" (Pp.ast_exp_to_string e);
      let value_t = eval_exp delta e in
      (match value_t with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi && cf ->
        dprintf "true branch";
        efse delta pi (s2@tl)
      | Ast.Int(bi, Reg 1) when bi_is_one bi && cf ->
        dprintf "false branch";
        efse delta pi (s1@tl)
      | _ ->
        dprintf "symbolic branch";
        let delta1,pi_t = efse delta value_t s1 in
        let delta2,pi_f = efse delta (Ast.exp_not value_t) s2 in
        let mergedelta,pi_t,pi_f = D.merge_super delta1 delta2 pi_t pi_f mode in
        let deltatl,pitl = efse mergedelta Ast.exp_true tl in
        deltatl, Ast.exp_and (Ast.exp_and pi (Ast.exp_or pi_t pi_f)) pitl)
  in
  let _,pi = efse (D.create ()) pi p in
  pi

(** Efficient fse algorithm for passified programs.

    Avoid putting concrete assignments in formulas version 2.
*)
let efse_lazy ?(cf=true) p pi mode =
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  (* let eval delta e = if cf then ( *)
  (*   let x = D.simplify delta e in *)
  (*   if Symbeval.is_concrete_mem_or_scalar x then x *)
  (*   else Symbeval.Symbolic e *)
  (* ) else Symbeval.Symbolic e *)
  (* in *)
  let eval_exp delta e = unwrap_symb (eval delta e) in
  let module VH = Var.VarHash in
  let h = VH.create 1000 in
  let merged v =
    (* Be careful when v is in the post/pre-condition *)
    if not (VH.mem h v) then
      VH.replace h v false
  (* Is v needed in formula *)
  and needed v =
    try VH.find h v
    with Not_found -> false
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
  let rec efse delta pi = function
    | [] -> dprintf "empty"; delta, pi
    | Assign(v, e) as s::tl ->
      dprintf "assign";
      let value = eval delta e in
      dprintf "stmt: %s\nevaluated %s to %s, concrete = %b" (stmt_to_string s) (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (unwrap_symb value)) (Symbeval.is_concrete_mem_or_scalar value);
      let delta',pi' = match value with
          (* Note: Even concrete assignments need to be added to the
             formula.  This is because when Ite merged two contexts,
             conflicting concrete assignments will be expunged from the
             concrete context.  In this case, the assignment in the
             formula must be used.

             However, we need to be careful about concrete memories,
             because they aren't really constant size. So, we won't add
             the evaluated memory to the formula.  *)
        | Symbeval.Symbolic(Ast.Int _) when cf -> D.set delta v value, lazy (
          if needed v then
            Assign.add_passive_value (Lazy.force pi) v value mode
          else Lazy.force pi)
         (* | Symbeval.ConcreteMem _ when cf -> D.set delta v value, lazy ( *)
         (*   if needed v then *)
         (*     Assign.add_passive_assignment (Lazy.force pi) v e mode *)
         | Symbeval.ConcreteMem _ when cf -> D.set delta v value, lazy (
           if needed v then
             Assign.add_passive_value (Lazy.force pi) v value mode
           else Lazy.force pi)
        | _ -> 
          used_vars_in e;
          delta, lazy (Assign.add_passive_assignment (Lazy.force pi) v e mode)
      in
      dprintf "done.";
      efse delta' pi' tl
    | Assert e::tl ->
      dprintf "assert";
      let value = eval_exp delta e in
      dprintf "Evaluated %s to %s" (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string value);
      (match value with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi && cf ->
        (* Assert false = false, pi \land false = false *)
        delta, lazy Ast.exp_false
      | Ast.Int(bi, Reg 1) when bi_is_one bi && cf ->
        (* Assert true = true, pi \land true = pi *)
        efse delta pi tl
      | _ ->
        let pi' = lazy (Ast.exp_and (Lazy.force pi) value) in
        used_vars_in e;
        efse delta pi' tl)
    | Ite(e, s1, s2)::tl ->
      dprintf "ite condition %s" (Pp.ast_exp_to_string e);
      let value_t = eval_exp delta e in
      (match value_t with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi && cf ->
        dprintf "true branch";
        efse delta pi (s2@tl)
      | Ast.Int(bi, Reg 1) when bi_is_one bi && cf ->
        dprintf "false branch";
        efse delta pi (s1@tl)
      | _ ->
        dprintf "symbolic branch";
        used_vars_in e;
        let delta1,pi_t = efse delta (lazy value_t) s1 in
        let delta2,pi_f = efse delta (lazy (Ast.exp_not value_t)) s2 in
        dprintf "merge time";
        let mergedelta,badlist = D.merge delta1 delta2 in
        (* Mark each variable in badlist as needing to go in the formula *)
        List.iter merged badlist;
        let deltatl,pitl = efse mergedelta (lazy Ast.exp_true) tl in
        deltatl, lazy (Ast.exp_and (Ast.exp_and (Lazy.force pi) (Ast.exp_or (Lazy.force pi_t) (Lazy.force pi_f))) (Lazy.force pitl)))
  in
  used_vars_in pi;
  let _,pi = efse (D.create ()) (lazy pi) p in
  Lazy.force pi

(* (\** Efficient fse algorithm for passified programs with feasibility testing. *\) *)
(* let efse_feas ?(cf=true) p pi = *)
(*   let eval delta e = if cf *)
(*     then D.simplify delta e *)
(*     else Symbeval.Symbolic e *)
(*   in *)
(*   let eval_exp delta e = unwrap_symb (eval delta e) in *)
(*   let rec efse delta pi solver stmts = *)
(*     (match stmts with *)
(*     | stmt::_ -> *)
(*       dprintf "Executing %s" (stmt_to_string stmt) *)
(*     | _ -> ()); *)
(*     match stmts with *)
(*     | [] -> delta, pi *)
(*     | Assign(v, e) as s::tl -> *)
(*       let value = eval delta e in *)
(*       dprintf "stmt: %s\nevaluated %s to %s, concrete = %b" (stmt_to_string s) (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (unwrap_symb value)) (Symbeval.is_concrete_mem_or_scalar value); *)
(*       let delta', pi' = *)
(*         let new_constraint = Ast.exp_eq (Ast.Var v) (unwrap_symb value) in *)
(*         dprintf "new constraint %s" (Pp.ast_exp_to_string new_constraint); *)
(*         let () = solver#add_constraint new_constraint in *)
(*         let new_pi = Ast.exp_and pi new_constraint in *)
(*         if Symbeval.is_concrete_mem_or_scalar value then *)
(*           (\* Note: Even concrete assignments need to be added to the *)
(*              formula.  This is because when Ite merged two contexts, *)
(*              conflicting concrete assignments will be expunged from the *)
(*              concrete context.  In this case, the assignment in the *)
(*              formula must be used. *\) *)
(*           D.set delta v value, new_pi *)
(*         else *)
(*           delta, new_pi *)
(*       in *)
(*       efse delta' pi' solver tl *)
(*     | Assert e::tl -> *)
(*       let value = eval_exp delta e in *)
(*       (match value with *)
(*       | Ast.Int(bi, Reg 1) when bi_is_zero bi -> *)
(*         (\* Assert false = false, pi \land false = false *\) *)
(*         delta, Ast.exp_false *)
(*       | Ast.Int(bi, Reg 1) when bi_is_one bi -> *)
(*         (\* Assert true = true, pi \land true = pi *\) *)
(*         efse delta pi solver tl *)
(*       | _ -> *)
(*         let pi' = Ast.exp_and pi value in *)
(*         dprintf "adding constraint %s" (Pp.ast_exp_to_string value); *)
(*         solver#add_constraint value; *)
(*         if (not solver#is_sat) then ( *)
(*           dprintf "Unsatisfiable assertion, returning false"; *)
(*           delta, Ast.exp_false *)
(*         ) else *)
(*           efse delta pi' solver tl) *)
(*     | Ite(e, s1, s2)::tl -> *)
(*       let value_t = eval_exp delta e in *)
(*       let true_sat, false_sat = match value_t with *)
(*       | Ast.Int(bi, Reg 1) when bi_is_one bi -> *)
(*         Valid, Unsat *)
(*       | Ast.Int(bi, Reg 1) when bi_is_zero bi -> *)
(*         Unsat, Valid *)
(*       | _ -> *)
(*         solver#push; *)
(*         solver#add_constraint e; *)
(*         let t = solver#is_sat in *)
(*         dprintf "true branch sat %b" t; *)
(*         solver#pop; *)
(*         solver#push; *)
(*         solver#add_constraint (Ast.exp_not e); *)
(*         let f = solver#is_sat in *)
(*         dprintf "false branch sat %b" f; *)
(*         solver#pop; *)
(*         let convert = function | true -> Sat | false -> Unsat in *)
(*         convert t, convert f *)
(*       in *)
(*       match true_sat, false_sat with *)
(*       (\* If one branch condition is Unsat, then this means pi => e *)
(*          or pi => \lnot e, and we do not need to put e (or \lnot e) in *)
(*          the new pi. *\) *)
(*       | (Valid|Sat), Unsat -> *)
(*         dprintf "Only looking at true branch"; *)
(*         (\* if true then x else y == x *\) *)
(*         efse delta pi solver (s1@tl) *)
(*       | Unsat, (Valid|Sat) -> *)
(*         dprintf "Only looking at false branch"; *)
(*         (\* if false then x else y == y *\) *)
(*         efse delta pi solver (s2@tl) *)
(*       | (Valid|Sat), (Valid|Sat) -> *)
(*         solver#push; *)
(*         solver#add_constraint value_t; *)
(*         let delta_t, pi_t = efse delta value_t solver s1 in *)
(*         solver#pop; *)
(*         solver#push; *)
(*         solver#add_constraint (Ast.exp_not value_t); *)
(*         let delta_f, pi_f = efse delta (Ast.exp_not value_t) solver s2 in *)
(*         solver#pop; *)
(*         let new_constraint = Ast.exp_or pi_t pi_f in *)
(*         dprintf "Adding constraint %s" (Pp.ast_exp_to_string new_constraint); *)
(*         solver#add_constraint new_constraint; *)
(*         let delta_merged,_ = D.merge delta_t delta_f in *)
(*         let delta_tl, pi_tl = efse delta_merged Ast.exp_true solver tl in *)
(*         delta_tl, Ast.exp_and (Ast.exp_and pi new_constraint) pi_tl *)
(*       | Unsat, Unsat -> *)
(*         failwith "Efse: We should never reach here because this is an infeasible path" *)
(*   in *)
(*   let s = new Solver.Z3.solver in *)
(*   s#add_constraint pi; *)
(*   dprintf "initial sat %b" s#is_sat; *)
(*   let _,pi = efse (D.create ()) pi s p in *)
(*   pi *)

end

module VMBack = Make(VMDelta)
include VMBack

