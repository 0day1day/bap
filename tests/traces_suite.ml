open OUnit
open Pcre
open Ast

(* TODO: Make this more dynamic; accept file name from list or command line *)
let il_file = "C/test.il";;
let halted_file = il_file ^ ".halted";;

let rec find_fun ?(msg="") ranges name = match ranges with
  | [] -> assert_failure ("Could not find function "^name^msg)
  | (n,s,e)::rs -> if (n = name) then (s,e) else find_fun ~msg rs name;;

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
  | Int(i,_) -> if (i <> correct)
	then 
	  assert_failure 
		("Final value " ^ (Int64.to_string i) ^ " does not equal correct value "
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

let concrete_eval_setup _ =
  let out = open_out il_file in
  let pp = new Pp.pp_oc out in
  let prog = Asmir.open_program ~loud:false "C/test" in
  (* Find the start and end addresses of main *)
  let ranges = Asmir.get_function_ranges prog in
  let (start_addr,_) = find_fun ranges "main" in
  (* TODO: silence this output for tests *)
  let ir = Asmir.asmprogram_to_bap prog in

  (*Printf.printf "%s\t0x%Lx 0x%Lx\n" "main" start_addr end_addr;*)

  (* 1. Find last ret in main's range *)
  (* 2. Inject halt true after ret *)

  pp#ast_program ir;
  pp#close;

  (* TODO: replace this script with ml *)
  assert_command ~verbose:true "./inject_halt.pl" [];
  (ranges, start_addr);;


let concrete_eval_test (ranges, s) = 
  let prog = Parser.program_from_file halted_file in
  let ctx1 = Symbeval.concretely_execute ~s ~loud:false prog in
  let eax1 = 0x2aL in
  (* XXX This is not valid.  Instead find the address of the call *)
  let s = 0x80482e4L in 
  let ctx2 = Symbeval.concretely_execute ~s ~loud:false ~i prog in
  (* XXX For some reason -1L, max_int, nor min_int don't work here *)
  let eax2 = 0xFFFFFFFFL in
  let msg = " from check_functions" in
  check_functions msg ranges ["main"; "g"];
  check_eax ctx1 eax1;
  check_eax ctx2 eax2;;

let concrete_eval_tear_down _ = Sys.remove il_file; Sys.remove halted_file;;

let suite = "Traces" >:::
  [
	"concrete_eval_test" >::
	  (bracket concrete_eval_setup concrete_eval_test concrete_eval_tear_down);
  ]
