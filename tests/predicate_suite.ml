open OUnit
open Pcre
open Ast
open TestCommon

let test_file = "C/test";;
let stp_out = "stp_out.stp";;
let solver = ref (Smtexec.STP.si);;

let predicate_stp_setup _ =
  (* TODO: Silence Asmir output for tests *)
  let prog = Asmir.open_program test_file in
  let ranges = Asmir.get_function_ranges prog in
  let (start_g, end_g) = find_fun ~msg:" in predicate_stp_setup" ranges "g" in
  let g_ir = Asmir.asmprogram_to_bap_range prog start_g end_g in
  let g_ir = Hacks.ret_to_jmp g_ir in
  let g_cfg = Cfg_ast.of_prog g_ir in
  let g_cfg = Prune_unreachable.prune_unreachable_ast g_cfg in
  let g_cfg = Unroll.unroll_loops g_cfg in
  let g_cfg = Hacks.remove_backedges g_cfg in
  let g_cfg = Prune_unreachable.prune_unreachable_ast g_cfg in
  g_cfg;;


(* Copied from topredicate...what's the better thing to do here? *)
let rename_astexp f =
  let vis = object
    inherit Ast_visitor.nop
    method visit_rvar v =
      try `ChangeTo(f v)
      with Not_found -> `DoChildren
  end in
  Ast_visitor.exp_accept vis


let to_ssagcl cfg post =
  let cfg = Hacks.remove_backedges cfg in
  let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let p = rename_astexp tossa post in
  let cfg =
    let vars = Formulap.freevars p in
    Ssa_simp.simp_cfg ~liveout:vars ~usedc:true ~usesccvn:true cfg      
  in
  let cfg = Cfg_ssa.to_astcfg cfg in
  let gcl = Gcl.of_astcfg cfg in
  (gcl, p)


let predicate_stp_solve_test g_cfg = 
  let str = "R_EAX_5:u32 == 42:u32" in
  let post = Parser.exp_from_string str in
  let (gcl, post) = to_ssagcl g_cfg post in
  let wp = Wp.wp gcl post in
  let m2a = new Memory2array.memory2array_visitor () in
  let wp = Ast_visitor.exp_accept m2a wp in
  let foralls = [] in
  let pp = (((!solver)#printer) :> Formulap.fppf) in
  let oc = open_out stp_out in
  let p = pp oc in
  p#assert_ast_exp_with_foralls foralls wp;
  p#counterexample;
  p#close;
  (let r = (!solver)#solve_formula_file stp_out in
      match r with
	  | Smtexec.Invalid -> ()
	  | _ -> assert_failure 
		"Predocate solution was not Invalid (aka was not valid)");
;;


let predicate_stp_tear_down _ = Sys.remove stp_out;;


let suite = "Predicate" >:::
  [
	"predicate_stp_solve_test" >::
	  (bracket 
		 predicate_stp_setup predicate_stp_solve_test predicate_stp_tear_down);
  ]
