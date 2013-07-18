open OUnit

let open_program_test () =
  ignore (Asmir.open_program "asm/nop");;

let resolve_program_test p =
  let asmp = Asmir.open_program p in
  let funcs = Func_boundary.get_function_ranges asmp in
  let f (n,s,e) = ignore(Asmir_disasm.vsa_at asmp s) in
  List.iter f funcs

let run_resolve_test s () =
  resolve_program_test s

let suite = "Asmir" >:::
  [
    "open_program_test" >:: open_program_test;
    "resolve_test" >:: run_resolve_test "C/recover-hard";
    "resolve_test_opt" >:: run_resolve_test "C/recover-hard-opt";
    "resolve_test_pointer" >:: run_resolve_test "C/recover-hard-pointer";
  ]
