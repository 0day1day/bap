(* A module to perform trace analysis *)

open Symbeval
open Type
open Ast

module D = Debug.Make(struct let name = "TraceEval" and default=`Debug end)
open D

  (*let init () = VH.create 5*)
  let def = {name=""; mem=true; t=reg_1; index=0L; value=0L; taint=Untaint}
  let gdt = {name="R_GDT"; mem=false; t=reg_32; index=0L; value=100L; taint=Untaint}
  let ldt = {name="R_LDT"; mem=false; t=reg_32; index=0L; value=220L; taint=Untaint}
  let concrete: (string, Ast.exp) Hashtbl.t = Hashtbl.create 5 (*ref = ref (init())*)
  let concrete_mem: (int64, Ast.exp) Hashtbl.t = Hashtbl.create 5 (*ref = ref (init())*)


module TaintLookup = 
struct 
  let lookup_var delta var = 
    let name = Var.name var in
    try Symbolic(Hashtbl.find concrete name) 
    with Not_found ->
      Symbolic.lookup_var delta var
  let conc2symb = Symbolic.conc2symb
  let normalize = Symbolic.normalize
  let update_mem = Symbolic.update_mem 
  let lookup_mem mu index endian = 
    match index with
     | Int(n,_) ->
      (try Hashtbl.find concrete_mem n
       with Not_found ->
       Symbolic.lookup_mem mu index endian)
     | _ -> Symbolic.lookup_mem mu index endian
end

module Trace = Symbeval.Make(TaintLookup)(PartialSubst)

(*
  let shorten stmts =
    (* FIXME: For now we just locate the last jump *)
    let rec find_tainted_jump = function
      | [] -> failwith "no tainted jump located"
      | (Ast.Jmp (Ast.Var v,_))::rest -> (v,rest)
      | _::rest -> find_tainted_jump rest
    in
    let rec get_last_block acc = function
      | [] -> failwith "no block label"
      | (Ast.Label (_,[Context atts]) as l)::rest -> (atts, l::acc, rest)
      | s::rest -> get_last_block (s::acc) rest 
    in
    let revstmts = List.rev stmts in
    let _,_,revstmts = get_last_block [] revstmts in
    let _tvar, revstmts = find_tainted_jump revstmts in
    let atts,block,_ = get_last_block [] revstmts in
    Printf.printf "%s\n" atts ;
    let lexbuf = Lexing.from_string atts in
    let p = Grammar.program Lexer.token lexbuf in
    let extended = p@block@[Ast.Halt (Ast.exp_true,[])] in
    Eval.eval_and_print_contexts extended ;
    stmts
  *)  

  let rec get_next_address = function
   | [] -> raise Not_found
   | (Ast.Label ((Addr n),_))::_ -> 
       Name ("pc_"^(Int64.format "0x%Lx" n))
   | _::xs -> get_next_address xs

  let rec cjmps_to_asserts acc = function
   | [] -> List.rev acc
   | (Ast.CJmp (e,l1,l2,atts) as x)::xs ->
     pdebug (Pp.ast_exp_to_string e) ;
     let newstmt = 
    (match lab_of_exp l1 with
     | Some (Name s as a) when not (String.contains s 'L') ->
       (try if get_next_address xs = a 
            then [Ast.Assert(e,atts); Ast.Jmp(l1,[])]
            else [Ast.Assert(UnOp(NOT,e),atts); Ast.Jmp(l2,[])]
        with Not_found -> [Ast.Assert(UnOp(NOT,e),atts); Ast.Jmp(l2,[])] )
     | _ -> [x]
    )
     in
     cjmps_to_asserts (newstmt@acc) xs
   | x::xs -> cjmps_to_asserts (x::acc) xs

  let eval state = 
    try
     let stmt = Trace.inst_fetch state.sigma state.pc in
     (*pdebug (Pp.ast_stmt_to_string stmt) ; *)

     let typ_to_bytes = function Reg bits -> (bits+7)/8 | _ -> failwith "not a register" in
     let get_byte i v = Int64.logand (Int64.shift_right v ((i-1)*8)) 0xffL in
     let first_bind_to_ctx hash index value =
       if Hashtbl.mem hash index then ()
       else Hashtbl.add hash index value
     in
     let rec add_mem index value limit = function
      | 0 -> ()
      | n -> let byte = get_byte (limit-n+1) value in
             (*prerr_endline ((Int64.to_string index) ^ " -> " ^ (Pp.ast_exp_to_string (Int(byte,reg_8))));*)
             first_bind_to_ctx concrete_mem index (Int(byte,reg_8)) ;
             add_mem (Int64.succ index) value limit (n-1)
     in
     let add_to_conc {name=name; mem=mem; index=index; value=value; t=typ; taint=taint} =
      if taint = Untaint then
       if mem then
        let limit = typ_to_bytes typ in
        add_mem index value limit limit 
       else
        first_bind_to_ctx concrete name (Int(value,typ))
     in
     let init_taint_tables () =
       Hashtbl.clear concrete;
       Hashtbl.clear concrete_mem;
       add_to_conc gdt;
       add_to_conc ldt
     in
     (match stmt with
       | Label (_,atts) -> 
         let conc_atts = List.filter (function Context _ -> true | _ -> false) atts in
         let conc_atts = List.map (function Context c -> c | _ -> def) conc_atts in
         if conc_atts != [] then init_taint_tables ();
         List.iter add_to_conc conc_atts
       | _ -> ()
     );
     pdebug (string_of_int state.pc) ;
     pdebug (Pp.ast_stmt_to_string stmt) ;
     Trace.eval_stmt state stmt
    with Failure fail -> 
      (try
      let prev = Trace.inst_fetch state.sigma (pred state.pc) in
      failwith (fail^"\nprev: "^(Pp.ast_stmt_to_string prev))
      with Failure _ -> failwith (fail^" no prev"))
         | Halted _ -> 
    let oc = open_out "predicate.txt" in
    let wp = state.pred in
    let m2a = new Memory2array.memory2array_visitor () in
    let wp = Ast_visitor.exp_accept m2a wp in
    let foralls = List.map (Ast_visitor.rvar_accept m2a) [] in
    let p = new Stp.pp_oc oc in
    let () = p#assert_ast_exp_with_foralls foralls wp true in
    p#close;
              failwith "success!!!!"
  
let rec unbounded_dfs st = 
  List.iter unbounded_dfs (eval st)

let dfs_ast_program p = 
  let p = Util.take p 253 in
  let no_jumps = List.filter (function Ast.Jmp _ | Special _ -> false | _ -> true) p in
  let no_cjumps = cjmps_to_asserts [] no_jumps in
  let trace = no_cjumps@[Ast.Halt (exp_true, [])] in
  let ctx = Symbolic.build_default_context trace in
  unbounded_dfs ctx
