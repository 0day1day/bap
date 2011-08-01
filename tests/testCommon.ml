open OUnit
open Pcre
open Ast


let rec find_fun ?(msg="") ranges name = match ranges with
  | [] -> assert_failure ("Could not find function "^name^msg)
  | (n,s,e)::rs -> if (n = name) then (s,e) else find_fun ~msg rs name;;


let rec find_call prog = 
  match prog with
  | [] -> assert_failure "Could not find a call in the given function"
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


let check_int64_answer e correct = 
  match e with
  | Int(int,_) -> if (int <> correct)
	then 
	  assert_failure 
		("Final value in EAX " ^ (Int64.to_string int) 
		 ^ " does not equal correct value "
		^ (Int64.to_string correct))
	else ()
  | _ -> assert_failure ("Final value in EAX is not an Ast.Int!");;


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

