open OUnit
module VM = Vsa.VM

let nmeets = 50

let simple_test filename var v () =
  let p = Asmir.asmprogram_to_bap (Asmir.open_program filename) in
  let cfg = Cfg_ast.of_prog p in
  let cfg = Ast_cond_simplify.simplifycond_cfg cfg in
  let _df_in, df_out = Vsa.AlmostVSA.DF.worklist_iterate_widen ~nmeets cfg in
  let l = df_out (Cfg.AST.G.V.create Cfg.BB_Exit) in
  let v' = VM.find var l in
  assert_equal ~msg:(Printf.sprintf "Value set %s for %s was different than expected value %s" (Vsa.AbsEnv.value_to_string v') (Pp.var_to_string var) (Vsa.AbsEnv.value_to_string v)) v v'

let suite = "Vsa" >:::
  [
    "a_test" >:: simple_test "asm/vsa-a.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,1L,1L))]);
    "a_test2" >:: simple_test "asm/vsa-a2.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,20L,20L))]);
    "ae_test" >:: simple_test "asm/vsa-ae.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,1L,1L))]);
    "ae_test2" >:: simple_test "asm/vsa-ae2.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,19L,19L))]);
    "b_test" >:: simple_test "asm/vsa-b.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,20L,20L))]);
    "be_test" >:: simple_test "asm/vsa-be.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,21L,21L))]);
    "e_test" >:: simple_test "asm/vsa-e.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,1L,1L))]);
    (* We don't get exact results here, because the information
       propagates through a NEQ constraint, which we cannot represent in
       a SI *)
    "e_test2" >:: simple_test "asm/vsa-e2.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,1L,20L,21L))]);
    "g_test" >:: simple_test "asm/vsa-g.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,1L,1L))]);
    "ge_test" >:: simple_test "asm/vsa-ge.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,1L,1L))]);
    "l_test" >:: simple_test "asm/vsa-l.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,20L,20L))]);
    "le_test" >:: simple_test "asm/vsa-le.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,21L,21L))]);
    "ne_test" >:: simple_test "asm/vsa-ne.o" Disasm_i386.ecx (`Scalar [(Vsa.VS.global, (32,0L,20L,20L))]);
  ]
