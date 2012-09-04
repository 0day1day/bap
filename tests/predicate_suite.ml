open Ast
open Big_int_convenience
open OUnit
open Pcre
open Test_common
open Type
open Utils_common

let test_file = "C/test";;
let g_il = "g.il";;
let stp_out = "stp_out.stp";;
let solver = Smtexec.YICES.si;;

let basic_setup () =
  let () = check_stp_path () in
  let m2actx = Memory2array.create_state () in
  let prog = [
    CJmp(BinOp(EQ, Var Disasm_i386.eax, Int(bi0, Reg 32)), Lab("L1"), Lab("L2"), []);
    Ast.Label(Name("L1"), []);
    Move(Disasm_i386.eax, Int(biconst 41, Reg 32), []);
    Jmp(Lab("end"), []);
    Ast.Label(Name("L2"), []);
    Move(Disasm_i386.eax, Int(biconst 42, Reg 32), []);
    Ast.Label(Name("end"), []);
  ] in
  typecheck prog;
  let cfg = Cfg_ast.of_prog prog in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  cfg, m2actx;;

let c_setup () =
  let _ = check_stp_path () in
  (* Silence floating point warnings for tests *)
  let _ = if (Asmir.get_print_warning()) then Asmir.set_print_warning(false) in
  let m2actx = Memory2array.create_state () in
  let prog = Asmir.open_program test_file in
  let ranges = Asmir.get_function_ranges prog in
  let (start_g, end_g) = find_fun ~msg:" in C_setup" ranges "g" in
  let g_ir = Asmir.asmprogram_to_bap_range prog start_g end_g in
  let g_ir = Hacks.ret_to_jmp g_ir in
  (* ret_to_jmp introduces a TMem, so mem2array must come after! *)
  let g_ir = Memory2array.coerce_prog_state m2actx g_ir in
  let g_cfg = Cfg_ast.of_prog g_ir in
  let g_cfg = Prune_unreachable.prune_unreachable_ast g_cfg in
  let g_cfg = Unroll.unroll_loops g_cfg in
  let g_cfg = Hacks.remove_cycles g_cfg in
  typecheck g_ir;
  g_cfg, m2actx;;


let test_post testname post stp_result (g_cfg, m2actx) =
  let post = Memory2array.coerce_exp_state m2actx post in
  let test_vc (name,vc) =
    print_endline ("Testing "^testname^" with "^name^" VC algorithm");
    let vcout, foralls = Vc.vc_astcfg vc Vc.default_options g_cfg post in
    let foralls = List.map (Memory2array.coerce_rvar_state m2actx) foralls in
    let pp = ((solver#printer) :> Formulap.fppf) in
    let oc = open_out stp_out in
    let p = pp oc in
    p#assert_ast_exp_with_foralls foralls vcout;
    p#counterexample;
    p#close;
    (let r = solver#solve_formula_file stp_out in
     if (r <> stp_result) then (
       assert_failure ("Predicate solution for " ^ name
                       ^ " was not "
                       ^(Smtexec.result_to_string stp_result)
                       ^" but "^(Smtexec.result_to_string r))))
  in
  (* Test with each VC algorithm *)
  List.iter test_vc Vc.pred_vclist;
;;


let predicate_stp_tear_down _ = 
  rm_and_ignore_list [g_il ; stp_out];;


let suite = "Predicate" >:::
  [
    "predicate_basic_solve_test" >::
      (bracket
	 basic_setup
	 (test_post "basic_solve" (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 42, Reg 32))) (Smtexec.Invalid))
	 predicate_stp_tear_down);
    "predicate_basic_unsolve_test" >::
      (bracket
	 basic_setup
	 (test_post "basic_unsolve"  (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 2, Reg 32))) (Smtexec.Valid))
	 predicate_stp_tear_down);
    "predicate_C_solve_test" >::
      (bracket
         c_setup
         (test_post "C_solve" (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 42, Reg 32))) (Smtexec.Invalid))
         predicate_stp_tear_down);
    "predicate_C_unsolve_test" >::
      (bracket
         c_setup
         (test_post "C_unsolve" (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 43, Reg 32))) (Smtexec.Valid))
         predicate_stp_tear_down);
  ]
