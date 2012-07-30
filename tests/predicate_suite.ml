open OUnit
open Pcre
open Ast
open Test_common
open Utils_common

let test_file = "C/test";;
let g_il = "g.il";;
let stp_out = "stp_out.stp";;
let solver = ref (Smtexec.STP.si);;

let predicate_stp_setup _ =
  let _ = check_stp_path() in 
  (* Silence floating point warnings for tests *)
  let _ = if (Asmir.get_print_warning()) then Asmir.set_print_warning(false) in
  let prog = Asmir.open_program test_file in
  let ranges = Asmir.get_function_ranges prog in
  let (start_g, end_g) = find_fun ~msg:" in predicate_stp_setup" ranges "g" in
  let g_ir = Asmir.asmprogram_to_bap_range prog start_g end_g in
  let g_ir = Hacks.ret_to_jmp g_ir in
  let g_cfg = Cfg_ast.of_prog g_ir in
  let g_cfg = Prune_unreachable.prune_unreachable_ast g_cfg in
  let g_cfg = Unroll.unroll_loops g_cfg in
  let g_cfg = Hacks.remove_cycles g_cfg in
  ignore(typecheck g_ir);
  g_cfg;;


let predicate_stp_solve_test str stp_result g_cfg =
  let post,_ = Parser.exp_from_string str in
  let (gcl, post) = to_ssagcl g_cfg post in
  let wp = Wp.wp gcl post in
  let mem_hash = Memory2array.create_state () in
  let wp = Memory2array.coerce_exp_state mem_hash wp in
  let foralls = List.map (Memory2array.coerce_rvar_state mem_hash) [] in 
  let pp = (((!solver)#printer) :> Formulap.fppf) in
  let oc = open_out stp_out in
  let p = pp oc in
  p#assert_ast_exp_with_foralls foralls wp;
  p#counterexample;
  p#close;
  (let r = (!solver)#solve_formula_file stp_out in
   if (r <> stp_result) then (assert_failure 
		("Predicate solution was not "^(Smtexec.result_to_string stp_result)^
			" but "^(Smtexec.result_to_string r))));
;;


let predicate_stp_tear_down _ = 
  rm_and_ignore_list [g_il ; stp_out];;


let suite = "Predicate" >:::
  [
	"predicate_stp_solve_test" >::
	  (bracket 
		 predicate_stp_setup 
		 (predicate_stp_solve_test "R_EAX:u32 == 42:u32" (Smtexec.Invalid))
		 predicate_stp_tear_down);
	"predicate_stp_unsolve_test" >::
	  (bracket 
		 predicate_stp_setup 
		 (predicate_stp_solve_test "R_EAX:u32 == 43:u32" (Smtexec.Valid))
		 predicate_stp_tear_down);

  ]
