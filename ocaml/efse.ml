(** Implementation of [efse] algorithm from DWP paper. *)

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
    let gcl = Gcl.gclhelp_of_astcfg ?entry ?exit cfg in
    dprintf "gcl: %s" (Gcl.gclhelp_to_string gcl);
    let fse = cgcl_to_fse (Gcl.gclhelp_of_astcfg ?entry ?exit cfg) in
    dprintf "fse: %s" (prog_to_string fse);
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


let unwrap_symb = function
  | Symbeval.Symbolic e -> e
  | Symbeval.ConcreteMem(m,v) -> Symbeval.symb_to_exp (Symbeval.conc2symb m v)

module type Delta =
sig
  type t
  val create : unit -> t
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
            `ChangeTo (D.get_exp delta v)
          with Not_found ->
            `DoChildren)
        | _ -> `DoChildren
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
    fse_unpass (D.create ()) post p

(** Inefficient fse algorithm for passified programs. *)
let fse_pass ?(cf=true) p post =
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
        else delta, Ast.exp_and pi (Ast.exp_eq (Ast.Var v) (unwrap_symb value))
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
  fse_pass (D.create ()) post p

(** Efficient fse algorithm for passified programs. *)
let efse ?(cf=true) p pi =
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
    | [] -> pi
    | Assign(v, e) as s::tl ->
      let value = eval delta e in
      dprintf "stmt: %s\nevaluated %s to %s, concrete = %b" (stmt_to_string s) (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (unwrap_symb value)) (Symbeval.is_concrete_mem_or_scalar value);
      let delta',pi' = if Symbeval.is_concrete_mem_or_scalar value
        then D.set delta v value, pi
        else delta, Ast.exp_and pi (Ast.exp_eq (Ast.Var v) (unwrap_symb value)) in
      efse delta' pi' tl
    | Assert e::tl ->
      let value = eval_exp delta e in
      dprintf "Evaluated %s to %s" (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string value);
      (match value with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi ->
        (* Assert false = false, pi \land false = false *)
        Ast.exp_false
      | Ast.Int(bi, Reg 1) when bi_is_one bi ->
        (* Assert true = true, pi \land true = pi *)
        efse delta pi tl
      | _ ->
        let pi' = Ast.exp_and pi value in
        efse delta pi' tl)
    | Ite(e, s1, s2)::tl ->
      let value_t = eval_exp delta e in
      (match value_t with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi ->
        efse delta pi (s2@tl)
      | Ast.Int(bi, Reg 1) when bi_is_one bi ->
        efse delta pi (s1@tl)
      | _ ->
        let pi_t = efse delta e s1 in
        let pi_f = efse delta (Ast.exp_not e) s2 in
        Ast.exp_and (Ast.exp_and pi (Ast.exp_or pi_t pi_f)) (efse delta Ast.exp_true tl))
  in
  efse (D.create ()) pi p

(** Efficient fse algorithm for passified programs with feasibility testing. *)
let efse_feas ?(cf=true) p pi =
  if debug then Pp.output_varnums := true;
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  let eval_exp delta e = unwrap_symb (eval delta e) in
  let rec efse delta pi solver stmts =
    (match stmts with
    | stmt::_ ->
      dprintf "Executing %s" (stmt_to_string stmt)
    | _ -> ());
    match stmts with
    | [] -> pi
    | Assign(v, e) as s::tl ->
      let value = eval delta e in
      dprintf "stmt: %s\nevaluated %s to %s, concrete = %b" (stmt_to_string s) (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (unwrap_symb value)) (Symbeval.is_concrete_mem_or_scalar value);
      let delta',pi' = if Symbeval.is_concrete_mem_or_scalar value
        then D.set delta v value, pi
        else
          let new_constraint = Ast.exp_eq (Ast.Var v) (unwrap_symb value) in
          dprintf "new constraint %s" (Pp.ast_exp_to_string new_constraint);
          let () = solver#add_constraint new_constraint in
          delta, Ast.exp_and pi new_constraint in
      efse delta' pi' solver tl
    | Assert e::tl ->
      let value = eval_exp delta e in
      (match value with
      | Ast.Int(bi, Reg 1) when bi_is_zero bi ->
        (* Assert false = false, pi \land false = false *)
        Ast.exp_false
      | Ast.Int(bi, Reg 1) when bi_is_one bi ->
        (* Assert true = true, pi \land true = pi *)
        efse delta pi solver tl
      | _ ->
        let pi' = Ast.exp_and pi value in
        dprintf "adding constraint %s" (Pp.ast_exp_to_string value);
        solver#add_constraint value;
        if (not solver#is_sat) then (
          dprintf "Unsatisfiable assertion, returning false";
          Ast.exp_false
        ) else
          efse delta pi' solver tl)
    | Ite(e, s1, s2)::tl ->
      let true_sat, false_sat = match eval_exp delta e with
      | Ast.Int(bi, Reg 1) when bi_is_one bi ->
        Valid, Unsat
      | Ast.Int(bi, Reg 1) when bi_is_zero bi ->
        Unsat, Valid
      | _ ->
        solver#push;
        solver#add_constraint e;
        let t = solver#is_sat in
        dprintf "true branch sat %b" t;
        solver#pop;
        solver#push;
        solver#add_constraint (Ast.exp_not e);
        let f = solver#is_sat in
        dprintf "false branch sat %b" f;
        solver#pop;
        let convert = function | true -> Sat | false -> Unsat in
        convert t, convert f
      in
      match true_sat, false_sat with
      (* If one branch condition is Unsat, then this means pi => e
         or pi => \lnot e, and we do not need to put e (or \lnot e) in
         the new pi. *)
      | (Valid|Sat), Unsat ->
        dprintf "Only looking at true branch";
        (* if true then x else y == x *)
        efse delta pi solver (s1@tl)
      | Unsat, (Valid|Sat) ->
        dprintf "Only looking at false branch";
        (* if false then x else y == y *)
        efse delta pi solver (s2@tl)
      | (Valid|Sat), (Valid|Sat) ->
        solver#push;
        let pi_t = efse delta e solver s1 in
        solver#pop;
        solver#push;
        let pi_f = efse delta (Ast.exp_not e) solver s2 in
        solver#pop;
        let new_constraint = Ast.exp_or pi_t pi_f in
        dprintf "Adding constraint %s" (Pp.ast_exp_to_string new_constraint);
        solver#add_constraint new_constraint;
        Ast.exp_and (Ast.exp_and pi new_constraint) (efse delta Ast.exp_true solver tl)
      | Unsat, Unsat ->
        failwith "Efse: We should never reach here because this is an infeasible path"
  in
  let s = new Solver.Z3.solver in
  s#add_constraint pi;
  dprintf "initial sat %b" s#is_sat;
  efse (D.create ()) pi s p

end

module VMBack = Make(VMDelta)
include VMBack

