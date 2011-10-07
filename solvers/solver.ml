(** A module to convert AST formulas to expressions for SMT Solvers.
    currently we support the following solvers.

    - Yices
    - STP
    - Z3

    @author Thanassis Avgerinos, Sang Kil Cha
*)

open Printf
open Ast
open Type
open Var

module D = Debug.Make(struct let name = "SOLVER" and default=`Debug end)
open D

(* move to helpers *)
let byte_array_to_string ar =
  let bitlength = Array.length ar in
  let s = String.create bitlength in
  Array.iteri (fun i c -> s.[bitlength - i - 1] <- (char_of_int (c+0x30))) ar;
  s

(* get bit width from the Int exp *)
let get_bits = Typecheck.bits_of_width

let timeout = ref 0

(* A Generic Interface for all solvers *)
module type Solver =
sig

  type set (* solver specific data structure *)
  type exp (* expression type of solver language *)

  val name : string (* name of the solver *)

  val mk_set  : unit -> set            (* initialize solver data set *)
  val del_set : set -> unit            (* delete solver data set *)
  val convert : set -> Ast.exp -> exp  (* covert Ast.exp to solver's exp *)
  val assert_true : set -> exp -> unit
  val is_true : set -> bool
  val pp_sol  : set -> unit            (* print solution *)
  val pp_exp  : set -> exp -> unit     (* expression printer *)

end

(* A Z3 InterFace module *)
module ZIF =
struct

  open Z3

  type exp = ast

  type set = {
    ctx : context;
    mutable vars : exp VarHash.t;
  }

  (*let is_valid {ctx=ctx} = assert (check ctx = L_TRUE)*)

  let name = "Z3"

  let mk_set () =
    let cfg = mk_config () in
    set_param_value cfg "SOFT_TIMEOUT" (string_of_int !timeout);
      (*set_param_value cfg "MODEL_ON_TIMEOUT" "true";*)
    set_param_value cfg "MODEL" "true";
    let ctx = mk_context cfg in
    {ctx = ctx; vars=VarHash.create 256}

  let del_set {ctx=ctx; vars=vars;} =
    del_context ctx;
    VarHash.clear vars

  let bool_to_bv ctx cond check =
    let typ = mk_bv_sort ctx 1 in
    let tr = mk_int ctx 1 typ
    and fl = mk_int ctx 0 typ in
    if check then mk_ite ctx cond tr fl
    else mk_ite ctx cond fl tr

  (* Converting AST binop expression to Z3 expressions *)
  let convert_binop ctx e1 e2 =
    function
    | PLUS    -> mk_bvadd ctx e1 e2
    | MINUS   -> mk_bvsub ctx e1 e2
    | TIMES   -> mk_bvmul ctx e1 e2
    | DIVIDE  -> failwith "unsupport divide"
    | SDIVIDE -> failwith "unsupport sdivide"
    | MOD     -> failwith "unsupport mod"
    | SMOD    -> failwith "unsupport smod"
    | LSHIFT  -> mk_bvshl ctx e1 e2
    | RSHIFT  -> mk_bvlshr ctx e1 e2
    | ARSHIFT -> mk_bvashr ctx e1 e2
    | AND     -> mk_bvand ctx e1 e2
    | OR      -> mk_bvor ctx e1 e2
    | XOR     -> mk_bvxor ctx e1 e2
    | EQ      -> bool_to_bv ctx (mk_eq ctx e1 e2) true
    | NEQ     -> bool_to_bv ctx (mk_eq ctx e1 e2) false
    | LT      -> bool_to_bv ctx (mk_bvult ctx e1 e2) true
    | LE      -> bool_to_bv ctx (mk_bvule ctx e1 e2) true
    | SLT     -> bool_to_bv ctx (mk_bvslt ctx e1 e2) true
    | SLE     -> bool_to_bv ctx (mk_bvsle ctx e1 e2) true

  let convert_unop {ctx = ctx} e =
    function
    | NOT -> mk_bvnot ctx e
    | NEG -> mk_bvneg ctx e

  let convert_cast {ctx = ctx} e (t,tnew) =
    (*dprintf "increasing bit size from %d to %d" (get_bits t) (get_bits tnew);*)
    function
      | _ when tnew = t -> e
      | CAST_UNSIGNED -> mk_zero_ext ctx (get_bits tnew - get_bits t) e
      | CAST_SIGNED -> mk_sign_ext ctx (get_bits tnew - get_bits t) e
      | CAST_HIGH -> mk_extract ctx (get_bits t-1) (get_bits t - get_bits tnew) e
      | CAST_LOW -> mk_extract ctx (get_bits tnew - 1) 0 e

  (* Converting AST binop expression to Z3 expressions *)
  let rec convert ({ctx=ctx; vars=vars} as set) =
    function
    | Var v ->
        let name = mk_string_symbol ctx (Var.name v) in
        let typ = mk_bv_sort ctx (get_bits (Var.typ v)) in
        let var = mk_const ctx name typ in
          (*dprintf "VAR: %s" (ast_to_string ctx var);*)
        VarHash.replace vars v var;
        var
    | Int (n, t) ->
        let typ = mk_bv_sort ctx (get_bits t) in
          mk_numeral ctx (Int64.to_string n) typ
    | BinOp (op, e1, e2) ->
        let e1 = convert set e1 in
        let e2 = convert set e2 in
          convert_binop ctx e1 e2 op
    | Cast (ctype, t, e) ->
        let typ = Typecheck.infer_ast e in
        let e = convert set e in
          convert_cast set e (typ,t) ctype
    | UnOp (op, e) ->
        let e = convert set e in
          convert_unop set e op
    | _ -> failwith "unsupported expression"

  (* Printing a Z3 formula *)
  let pp_exp {ctx=ctx} formula =
    dprintf "%s" (ast_to_string ctx formula);
    flush stderr

(*  let maybe_false {ctx = ctx} cond =
    let negcond = match sort_to_string ctx (get_sort ctx cond) with
      | "bool" -> mk_not ctx cond
      (*| "bv"   -> mk_bvnot ctx cond*)
      | _ -> failwith "omg?"
    in
    push ctx;
    assert_cnstr ctx negcond;
    let answer = match check ctx with
      | L_TRUE -> true
      | L_FALSE -> false
      | _ -> failwith "Unable to determine validity"
    in
    Printf.printf "chk2\n"; flush stdout;
    pop ctx 1;
    Printf.printf "chk3\n"; flush stdout;
    answer
*)

  let assert_true {ctx=ctx} formula =
    let typ = mk_bv_sort ctx 1 in
    let tr = mk_int ctx 1 typ in
    let formula = mk_eq ctx formula tr in
    (*pp_exp c formula; flush stderr;*)
    assert_cnstr ctx formula
    (*with Failure s -> dprintf "shitty...%s" s*)

  let store_vars = ref (VarHash.create 1)

  let push {ctx = ctx; vars = vars} =
    store_vars := VarHash.copy vars;
    push ctx

  let pop context =
    pop context.ctx 1;
    context.vars <- !store_vars

  let maybe_true ({ctx = ctx} as set) cond =
    push set;
    let cond = convert set cond in
    assert_true set cond;
    let answer = match check ctx with
      | L_TRUE -> true
      | L_FALSE -> false
      | _ -> failwith "Unable to determine validity"
    in
    pop set;
    answer

  let get_concrete_model {ctx = ctx} =
    let sat, model = check_and_get_model ctx in
    if sat <> L_TRUE then
      (
        del_model ctx model;
        failwith "Unsatisfiable Path Predicate!!"
      )
    else
      model

  let get_concrete_value ctx model expr =
    match eval ctx model expr with
      | true, v ->
          (
            match get_ast_kind ctx v with
              | NUMERAL_AST -> get_numeral_uint ctx v
              | APP_AST -> wprintf "Found an unbound variable, using 0"; (true, 0)
              | VAR_AST -> failwith "bound variable!"
              | QUANTIFIER_AST -> failwith "variable had a quantifier type"
              | UNKNOWN_AST -> failwith "unknown type"
          )
      | _ -> failwith "Failed to get the conrete value!"

(*
    let answer = match check_assumptions ctx [| cond |] 2 [| |] with
      | L_TRUE, m,  f, unsat, _ when unsat = 0 ->
          Printf.printf "It is false: %s\n" (model_to_string ctx m); flush stdout;
          Printf.printf "value: %s\n" (ast_to_string ctx f); flush stdout;
          true
      | L_TRUE, _, _, unsat, _ when unsat > 0 ->
          Printf.printf "It is true\n"; flush stdout;
          false
      | _, m, _, _, _ ->
          failwith "Something bad happened"
    in
      answer
*)

  let get_bool_value ({ctx = ctx} as set) cond =
    (* Can we do it any other way??? *)
    let model = get_concrete_model set in
    (*dprintf "model: %s" (model_to_string ctx model);*)
    (*pp_exp set cond;*)
    let answer = match eval ctx model cond with
      | true, v when is_eq_ast ctx v (mk_int ctx 1 (mk_bv_sort ctx 1)) -> Some true
      | true, v when is_eq_ast ctx v (mk_int ctx 0 (mk_bv_sort ctx 1)) -> Some false
      | true, v ->
          pp_exp set v;
          dprintf "neither true nor false";
          None
      | _ -> failwith "Eval call failed"
    in
    del_model ctx model;
    answer

  let get_values ({ctx = ctx; vars = vars} as set) =
    let model = get_concrete_model set in
    let values = VarHash.fold
      (fun var value acc ->
         (var, snd(get_concrete_value ctx model value))::acc
      ) vars []
    in
    del_model ctx model;
    values

  let get_model ({ctx = ctx;} as set) form =
    push set;
    let form = convert set form in
    assert_true set form;
    let answer = get_values set in
    pop set;
    answer

  let is_true {ctx = ctx} = check ctx = L_TRUE

  (* Printing a Z3 solution *)
  let pp_sol {ctx=ctx} =
    match check_and_get_model ctx with
      | (L_TRUE,m) ->
          dprintf "satisfiable";
          Printf.printf "solution: %s\n" (model_to_string ctx m); flush stdout;
          del_model ctx m
      | (L_FALSE,_) ->
          dprintf "unsatisfiable"
      | (L_UNDEF,m) ->
          dprintf "unknown";
          del_model ctx m

end


(* A STP InterFace module *)
(*module SIF =
struct

  open Stpvc

  type context = vc

  type set = {
    ctx : context;
    (*vars : (var_decl * expr) VarHash.t;*)
  }

  type exp = Stpvc.exp

  let name = "STP"

  let mk_set () =
    let ctx = create_validity_checker () in
    {ctx = ctx}

  let del_set {ctx = ctx} =
    Libstp.vc_Destroy ctx

  (* Converting AST binop expression to STP expressions *)
  let convert_binop ctx e1 e2 typ = function
    | PLUS   -> e_bvplus ctx (get_bits typ) e1 e2
    | MINUS  -> e_bvminus ctx (get_bits typ) e1 e2
    | TIMES  -> e_bvmult ctx (get_bits typ) e1 e2
    | EQ     -> e_eq ctx e1 e2
    | _ -> failwith "unsupported"

  (* Converting AST binop expression to STP expressions *)
  let rec convert ({ctx=ctx} as set) = function
    | Var v ->
        let typ = bitvector_t ctx (get_bits (Var.typ v)) in
        let var = e_var ctx (Var.name v) typ in
          var
    | Int (n, t) ->
        e_bv_of_int64 ctx (get_bits t) n
    | BinOp (op, e1, e2) ->
        let typ = Typecheck.infer_ast e1 in
        let e1 = convert set e1 in
        let e2 = convert set e2 in
          convert_binop ctx e1 e2 typ op
    | _ -> failwith "unsupported expression"

  (*Printing an STP formula *)
  let pp_exp _ formula =
    dprintf "%s" (to_string formula)

  let assert_true {ctx=ctx} = do_assert ctx

  let is_true {ctx = ctx} = query ctx (e_false ctx)

  (* Printing an STP solution *)
  let pp_sol {ctx=ctx} =
    match query ctx (e_false ctx) with
      | false ->
          dprintf "satisfiable";
          Libstp.vc_printCounterExample ctx;
          print_newline ()
      | true ->
          dprintf "unsatisfiable"

end
*)

module Make(S: Solver) =
struct

  let solve formula =
    dprintf "Starting up %s" S.name;
    dprintf "Formula Given: %s" (Pp.ast_exp_to_string formula);
    let ctx = S.mk_set() in              (* init the solver context *)
    let form = S.convert ctx formula in  (* convert given Ast.exp formula *)
    S.pp_exp ctx form;
    S.assert_true ctx form;              (* asserting the formula to be true *)
    S.pp_sol ctx;                        (* print out the solution *)
    S.del_set ctx                        (* delete the context *)

  let create_solver = S.mk_set
  let conv_formula = S.convert
  let assert_true = S.assert_true
  let pp_sol = S.pp_sol
  let is_true = S.is_true
  let destroy_solver = S.del_set

  let is_satisfiable formula =
    let ctx = S.mk_set () in
    let formula = S.convert ctx formula in
    S.pp_exp ctx formula;
    S.assert_true ctx formula ;
    let answer = S.is_true ctx in
    S.pp_sol ctx;                        (* print out the solution *)
    S.del_set ctx;
    answer

end

module Z3 = Make(ZIF)
(*module STP = Make(SIF)*)

let test1 =
  let b2 =  Var(newvar "x2" reg_8) in
  let b3 =  Var(newvar "x3" reg_8) in
  let b4 =  Var(newvar "x4" reg_8) in
  let result1 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT,
                                            Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(24L,reg_32)),
                                      BinOp(OR,
                                            Int(0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT,
                                                        Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(8L,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(24L,reg_32)),
                          Int(0xffL,reg_32)))),
          Int(24L,reg_32))
  in
  let result2 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(24L,reg_32)),
                                      BinOp(OR, Int(0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(8L,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(16L,reg_32)),
                          Int(0xffL,reg_32)))),
          Int(16L,reg_32))
  in
  let result3 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(24L,reg_32)),
                                      BinOp(OR, Int(0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(8L,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(8L,reg_32)),
                          Int(0xffL,reg_32)))),
          Int(8L,reg_32))
  in
  let result4 =
    Cast(CAST_UNSIGNED,
         reg_32,
         Cast(CAST_LOW,
              reg_8,
              BinOp(AND,
                    BinOp(OR,
                          BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                Int(24L,reg_32)),
                          BinOp(OR, Int(0x410000L,reg_32),
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                            Int(8L,reg_32)),
                                      Cast(CAST_UNSIGNED, reg_32, b2)))),
                    Int(0xffL,reg_32))))
  in
  let left1 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT,
                                            Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(24L,reg_32)),
                                      BinOp(OR, Int(0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(8L,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(24L,reg_32)),
                          Int(0xffL,reg_32)))),
          Int(24L,reg_32))
  in
  let left2 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT,
                                            Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(24L,reg_32)),
                                      BinOp(OR, Int(0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(8L,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(16L,reg_32)),
                          Int(0xffL,reg_32)))),
          Int(16L,reg_32))
  in
  let left3 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(24L,reg_32)),
                                      BinOp(OR, Int(0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(8L,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(8L,reg_32)),
                          Int(0xffL,reg_32)))),
          Int(8L,reg_32))
  in
  let left4 =
    Cast(CAST_UNSIGNED,
         reg_32,
         Cast(CAST_LOW,
              reg_8,
              BinOp(AND,
                    BinOp(OR,
                          BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                Int(24L,reg_32)),
                          BinOp(OR, Int(0x410000L,reg_32),
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                            Int(8L,reg_32)),
                                               Cast(CAST_UNSIGNED, reg_32, b2)))),
                    Int(0xffL,reg_32))))
  in
  let final =
    BinOp(LT,
          BinOp(OR, result1, BinOp(OR, result2, BinOp(OR, result3, result4))),
          BinOp(MINUS,
                BinOp(OR,
                      left1,
                      BinOp(OR,
                            left2,
                            BinOp(OR, left3, left4))),
                Int(0x400000L,reg_32)))
  in
    final

(* BinOp(EQ,
                  Int(0L, reg_32),
                  BinOp(AND,
                        Var(newvar "X" reg_32),
                        Int(0L, reg_32)))
            *)

let time solver solve test =
  let do_many f x =
    for i = 0 to 500 do
      f x
    done
  in
  let t = Unix.gettimeofday() in
  do_many solve test;
  let t' = Unix.gettimeofday() in
  Printf.eprintf "%s\t-- time: %f\n" solver (t' -. t)

class z3_solver formula =
  object(s)

    val c =
      let solver = Z3.create_solver () in
      let form = Z3.conv_formula solver formula in
      (*ZIF.pp_exp solver form;*)
      Z3.assert_true solver form;
      (*ZIF.is_valid solver;*)
      solver

    method is_sat cond =
      (*ZIF.pp_exp c cond;*)
      ZIF.maybe_true c cond

    method pp_sol () =
      Z3.pp_sol c;

    method get_values () =
      ZIF.get_values c

    method get_model form =
      ZIF.get_model c form

    method add constr =
      let constr = Z3.conv_formula c constr in
      (*ZIF.pp_exp c constr;*)
      Z3.assert_true c constr
      (*ZIF.is_valid c*)

    method destroy =
      Z3.destroy_solver c

  end

let testcases () =
  let s = new z3_solver test1 in
  let b = s#is_sat test1 in
  Printf.printf "IS SAT: %b\n" b;
  s#pp_sol ();
  ignore(s#is_sat exp_true);
  s#destroy

