open OUnit
open Pcre
open Ast

let il_file = "C/test.il";;


let rec find_fun ?(msg="") ranges name = match ranges with
  | [] -> assert_failure ("Could not find function "^name^msg)
  | (n,s,e)::rs -> if (n = name) then (s,e) else find_fun ~msg rs name;;


let rec find_call prog = 
  match prog with
  | [] -> assert_failure "Could not find a call in the main function"
  | p::ps ->
	match p with
	| Label(Type.Addr(a),attrs) -> 
	  (match attrs with
	  | [Type.Asm(asm)] -> 
		if (pmatch ~pat:"call" asm) then a else find_call ps
	  | _ -> find_call ps)
	| _ -> find_call ps;;


(* Return list of statments between start_addr and end_addr *)
let inject_stmt prog start_addr asm stmt = 
  let rec inject_stmt_k stmts starta asm_str inj_stmt k =
	match stmts with
	| [] -> 
	  assert_failure ("Could not find asmembly instruction "^asm_str^" in main")
	| s::[] -> 
	  assert_failure ("Could not find asmembly instruction "^asm_str^" in main")
	(* Match for the addr and label at the same time *)
	| s::l::ss -> 
	  match starta with
	  | Some(a) -> (match s with
		| Label(Type.Addr(addr),attrs) -> 
		    if (addr = a) then inject_stmt_k ss None asm_str inj_stmt (l::s::k)
			else inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
		| _ -> inject_stmt_k (l::ss) starta asm_str inj_stmt (s::k)
	  )
	  (* We are inside the desired block; find asm_str and inject inj_stmt *)
	  | None -> (match s with
		| Label(Type.Addr(addr),attrs) -> 
		  (match attrs with
		  | [Type.Asm(asm)] ->
			if (pmatch ~pat:"ret" asm) then (List.rev k)@(s::l::inj_stmt::ss)
			else inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
		  | _ -> inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
		  )
		| _ -> inject_stmt_k (l::ss) starta asm_str inj_stmt (s::k)
	  )
  in
  inject_stmt_k prog (Some(start_addr)) asm stmt [];;


let halt_stmt = Halt(exp_true,[]);;


(* i represents the change on the stack to the "wrong" value for function g *)
let i = 
  let a = Parser.exp_from_string "R_ESP_1:u32" in
  let e = Parser.exp_from_string "43:u32" in
  let t = Typecheck.infer_ast e in
  let m = match Parser.exp_from_string "mem_45:?u32" with
    | Var(v) -> v
    | _ -> assert false
  in
  let s = Move(m, Store(Var(m), a, e, exp_false, t), []) in
  [s];;


let check_int64_answer e correct = 
  match e with
  | Int(int,_) -> if (int <> correct)
	then 
	  assert_failure 
		("Final value " ^ (Int64.to_string int) 
		 ^ " does not equal correct value "
		^ (Int64.to_string correct))
	else ()
  | _ -> assert_failure ("Final value is not an Ast.Int!");;


let check_eax ctx eax =
  let pat = "R_EAX_" in
  Var.VarHash.iter 
	(fun k v ->
	  match k,v with
	  | var,Symbeval.Symbolic e ->
		if (pmatch ~pat (Pp.var_to_string var)) 
		then check_int64_answer e eax
		else ()
	  | _ -> ()
    ) ctx.Symbeval.delta;;


let check_functions msg ranges names =
  ignore(List.map (find_fun ~msg ranges) names);;


(** Lift C/test and convert it to bap.  Then inject "halt true" after the return
    in the main function. Print this out to the file test.il. *)
let concrete_eval_setup _ =
  let out = open_out il_file in
  let pp = new Pp.pp_oc out in
  let prog = Asmir.open_program ~loud:false "C/test" in
  let ranges = Asmir.get_function_ranges prog in
  let (start_addr,_) = find_fun ranges "main" in
  (* TODO: silence this output for tests *)
  let ir = Asmir.asmprogram_to_bap prog in
  let outir = inject_stmt ir start_addr "ret" halt_stmt in 
  pp#ast_program outir;
  pp#close;
  (ranges, start_addr);;


(** Open the file test.il and run two concrete executions.  The first verifies
    running from main results in the desired value (42 = 0x2aL).  The second
    concrete execution changes the value on the stack to 43 (i), starts
    the execution at the "call <g>" assembly instruction in main, and verifies
	that the result is -1. *)
let concrete_eval_test (ranges, s) = 
  let prog = Parser.program_from_file il_file in
  let ctx1 = Symbeval.concretely_execute ~s ~loud:false prog in
  let eax1 = 0x2aL in
  let (start_addr,end_addr) = find_fun ranges "main" in
  let main_prog = Ast_convenience.find_prog_chunk prog start_addr end_addr in
  let s = find_call main_prog in 
  let ctx2 = Symbeval.concretely_execute ~s ~loud:false ~i prog in
  let eax2 = Arithmetic.to64(-1L,Type.Reg(32)) in
  let msg = " from check_functions" in
  check_functions msg ranges ["main"; "g"];
  check_eax ctx1 eax1;
  check_eax ctx2 eax2;;


let concrete_eval_tear_down _ = Sys.remove il_file;;


let suite = "Traces" >:::
  [
	"concrete_eval_test" >::
	  (bracket concrete_eval_setup concrete_eval_test concrete_eval_tear_down);
  ]
