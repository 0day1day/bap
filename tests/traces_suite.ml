open OUnit
open Pcre

let halted_file = "C/test.il.halted";;

let rec find_fun ranges name = match ranges with
  | [] -> assert_failure ("Could not find function "^name)
  | (n,s,e)::rs -> if (n = name) then (s,e) else find_fun rs name

let check_answer e = 
  match e with
  | Ast.Int(i,_) -> if (i <> Int64.of_int(42))
	then 
	  assert_failure ("Final value does not = 42, but " ^ (Int64.to_string i))
	else ()
  | _ -> assert_failure ("Final value is not an int!")

let concrete_eval_setup _ =
  (*let out = open_out halted_file in
  let pp = new Pp.pp_oc out in*)
  let prog = Asmir.open_program ~loud:false "C/test" in
  (* Find the start and end addresses of main *)
  let ranges = Asmir.get_function_ranges prog in
  let (start_addr,end_addr) = find_fun ranges "main" in
  (*let ir = Asmir.asmprogram_to_bap prog in*)

  Printf.printf "%s\t0x%Lx 0x%Lx\n" "main" start_addr end_addr;

  (* 1. Find last ret in main's range *)
  (* 2. Inject halt true after ret *)

  (*pp#ast_program ir;
  pp#close;*)

  assert_command ~verbose:true "./inject_halt.pl" [];
  start_addr;;

let concrete_eval_tear_down _ = Sys.remove halted_file;;

let concrete_eval_test start_addr = 
  let prog = Parser.program_from_file halted_file in
  let ctx = Symbeval.concretely_execute prog ~s:start_addr in
  let pat = "R_EAX_" in
	Var.VarHash.iter 
	  (fun k v ->
		match k,v with
		| var,Symbeval.Symbolic e ->
		  if (pmatch ~pat (Pp.var_to_string var)) 
		  then 
			((Printf.printf 
				"%s\n" 
				((Pp.var_to_string var) ^ " = " ^ (Pp.ast_exp_to_string e)));
			 check_answer e)
		  else ()
		| _ -> ()
       ) ctx.Symbeval.delta;;

let suite = "Traces" >:::
  [
	"concrete_eval_test" >::
	  (bracket concrete_eval_setup concrete_eval_test concrete_eval_tear_down);
  ]
