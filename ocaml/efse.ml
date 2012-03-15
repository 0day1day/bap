(** Implementation of [efse] algorithm from DWP paper. *)

open Big_int_convenience
module CA = Cfg.AST
open Type
module VM = Var.VarMap
type var = Ast.var
type exp = Ast.exp

type stmt = | Assign of (var * exp)
            | Assert of exp
            | Ite of (exp * prog * prog)
and prog = stmt list

(* EFSE's lookup functions raise Not_found as soon as a variable would
   be Symbolic *)
module CFMemL =
struct
  open Ast
  open Symbeval
  include BuildSymbolicMemL(MemVMBackEnd)

  let lookup_var delta var =
    match lookup_var delta var with
    | Symbolic(Int(_)) as x -> x
    | ConcreteMem _ as x -> x
    | _ -> raise Not_found

  (* let rec update_mem mu pos value endian = *)
  (*   (\*pdebug "Update mem" ;*\) *)
  (*   match mu, pos with *)
  (*     | ConcreteMem (m,v), Int(p,t) -> *)
  (*         ConcreteMem(AddrMap.add (normalize p t) value m, v) *)
  (*     | _ -> failwith "Symbolic memory in concrete evaluation" *)

  let rec lookup_mem mu index endian =
    (*pdebug "Lookup mem" ;*)
    match lookup_mem mu index endian with
    | Int _ as x -> x
    | _ -> raise Not_found

end

module ConcreteMap = Symbeval.Make(CFMemL)(Symbeval.AlwaysEvalLet)(Symbeval.StdAssign)(Symbeval.StdForm)

let rec stmt_to_string = function
  | Assign(v,e) -> Printf.sprintf "%s = %s" (Var.name v) (Pp.ast_exp_to_string e)
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
          c e (fun ce -> c (Gcl.CSeq es) (fun ces -> k ce@ces))
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
    cgcl_to_fse (Gcl.gclhelp_of_astcfg ?entry ?exit cfg)

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
  val set : t -> var -> exp -> t
  (** Setter method *)
  val get : t -> var -> exp
  (** Getter method.  Raises [Not_found] exception if variable is
      not set. *)
  val simplify : t -> exp -> exp
  (** [simplify d e] simplifies exp in context d. *)
end

module VMDelta =
struct
  type t = Symbeval.varval VM.t
  let create () =
    VM.empty
  let set h v e =
    VM.add v (Symbeval.Symbolic e) h
  let unwrap_symb = function
    | Symbeval.Symbolic e -> e
    | Symbeval.ConcreteMem(m,v) -> Symbeval.symb_to_exp (Symbeval.conc2symb m v)
  let get h v =
    unwrap_symb (VM.find v h)
  let simplify d e =
    (* Reduce to constant if possible *)
    try (
      match unwrap_symb (ConcreteMap.eval_expr d e) with
      | Ast.Int _ as x -> x
      | _ -> raise Not_found
    ) with Not_found -> e
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
            `ChangeTo (D.get delta v)
          with Not_found ->
            `DoChildren)
        | _ -> `DoChildren
    end in
    Ast_visitor.exp_accept v e

(** Inefficient fse algorithm for unpassified programs. *)
  let fse_unpass ?(cf=true) p post =
    let eval delta e = if cf then sub_eval delta (D.simplify delta e) else sub_eval delta e in
    let rec fse_unpass delta pi = function
      | [] -> pi
      | Assign(v, e)::tl ->
        let value = eval delta e in
        let delta' = D.set delta v value in
        fse_unpass delta' pi tl
      | Assert(e)::tl ->
        let value = eval delta e in
        let pi' = Ast.exp_and pi value in
        fse_unpass delta pi' tl
      | Ite(e, s1, s2)::tl ->
        let value_t = eval delta e in
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
  let eval delta e = if cf then sub_eval delta (D.simplify delta e) else e in
  let rec fse_pass delta pi = function
    | [] -> pi
    | Assign(v, e)::tl ->
      let value = eval delta e in
      let delta',pi' = match value with
        | Ast.Int _ -> D.set delta v value (** Update for constants, no need to add to pi *),
          pi
        | _ -> delta, Ast.exp_and pi (Ast.exp_eq (Ast.Var v) value)
      in
      fse_pass delta' pi' tl
    | Assert(e)::tl ->
      let value = eval delta e in
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
      let value_t = eval delta e in
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
  let eval delta e = if cf then sub_eval delta (D.simplify delta e) else e in
  let rec efse delta pi = function
    | [] -> pi
    | Assign(v, e)::tl ->
      let value = eval delta e in
      let delta',pi' = match value with
        | Ast.Int _ -> D.set delta v value, pi
        | _ -> delta, Ast.exp_and pi (Ast.exp_eq (Ast.Var v) e) in
      efse delta' pi' tl
    | Assert e::tl ->
      let value = eval delta e in
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
      let value_t = eval delta e in
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

end

module VMBack = Make(VMDelta)
include VMBack

