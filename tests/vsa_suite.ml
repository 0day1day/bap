open OUnit
module VM = Vsa_ast.VM

let nmeets = 50

let ast_test filename var v () =
  let p = Asmir.asmprogram_to_bap (Asmir.open_program filename) in
  let cfg = Cfg_ast.of_prog p in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  let cfg = Ast_cond_simplify.simplifycond_cfg cfg in
  let _df_in, df_out = Vsa_ast.vsa ~nmeets cfg in
  let exiT = Cfg.AST.G.V.create Cfg.BB_Exit in
  let l = df_out (Vsa_ast.last_loc cfg exiT) in
  let l = BatOption.get l in
  let v' = VM.find var l in
  assert_equal ~msg:(Printf.sprintf "Value set %s for %s was different than expected value %s" (Vsa_ast.AbsEnv.value_to_string v') (Pp.var_to_string var) (Vsa_ast.AbsEnv.value_to_string v)) v v'

let ssa_test filename var v () =
  let p = Asmir.asmprogram_to_bap (Asmir.open_program filename) in
  let cfg = Cfg_ast.of_prog p in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  let cfg = Ast_cond_simplify.simplifycond_cfg cfg in
  let {Cfg_ssa.cfg; to_ssavar} = Cfg_ssa.trans_cfg ~tac:false cfg in
  Cfg_pp.SsaStmtsDot.output_graph (open_out "/tmp/wtf.dot") cfg;
  let var = to_ssavar var in
  let _df_in, df_out = Vsa_ssa.vsa ~nmeets cfg in
  let exiT = Cfg.SSA.G.V.create Cfg.BB_Exit in
  let l = df_out (Vsa_ssa.last_loc cfg exiT) in
  let l = BatOption.get l in
  let v' = VM.find var l in
  assert_equal ~msg:(Printf.sprintf "Value set %s for %s was different than expected value %s" (Vsa_ssa.AbsEnv.value_to_string v') (Pp.var_to_string var) (Vsa_ssa.AbsEnv.value_to_string v)) v v'


let make_ast_test (n, f, r, si) =
  n^"_ast" >:: ast_test f r (`Scalar [(Vsa_ast.VS.global, si)])

let make_ssa_test (n, f, r, si) =
  n^"_ssa" >:: ssa_test f r (`Scalar [(Vsa_ssa.VS.global, si)])

let tests =
  [
    "a_test", "asm/vsa-a.o", Disasm_i386.ecx, (32,0L,1L,1L);
    "a_test2", "asm/vsa-a2.o", Disasm_i386.ecx, (32,0L,20L,20L);
    "ae_test", "asm/vsa-ae.o", Disasm_i386.ecx, (32,0L,1L,1L);
    "ae_test2", "asm/vsa-ae2.o", Disasm_i386.ecx, (32,0L,19L,19L);
    "b_test", "asm/vsa-b.o", Disasm_i386.ecx, (32,0L,20L,20L);
    "be_test", "asm/vsa-be.o", Disasm_i386.ecx, (32,0L,21L,21L);
    "e_test", "asm/vsa-e.o", Disasm_i386.ecx, (32,0L,1L,1L);
    (* We don't get exact results here, because the information
       propagates through a NEQ constraint, which we cannot represent in
       a SI *)
    "e_test2", "asm/vsa-e2.o", Disasm_i386.ecx, (32,1L,20L,21L);
    "g_test", "asm/vsa-g.o", Disasm_i386.ecx, (32,0L,1L,1L);
    "ge_test", "asm/vsa-ge.o", Disasm_i386.ecx, (32,0L,1L,1L);
    "l_test", "asm/vsa-l.o", Disasm_i386.ecx, (32,0L,20L,20L);
    "le_test", "asm/vsa-le.o", Disasm_i386.ecx, (32,0L,21L,21L);
    "ne_test", "asm/vsa-ne.o", Disasm_i386.ecx, (32,0L,20L,20L);
    "mem_test", "asm/mem.o", Disasm_i386.ecx, (32,0L,42L,42L);
  ]

let suite = "Vsa" >:::
  List.map make_ast_test tests
  @ List.map make_ssa_test tests
