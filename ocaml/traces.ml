(* A module to perform trace analysis *)

open Symbeval
open Type
open Ast

module D = Debug.Make(struct let name = "TraceEval" and default=`Debug end)
open D

  (*let init () = VH.create 5*)
  let def = {name=""; mem=true; t=reg_1; index=0L; value=0L}
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

module Trace = Symbeval.Make(TaintLookup)

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
  let eval state = 
    try
     let stmt = Trace.inst_fetch state.sigma state.pc in
     (*pdebug (Pp.ast_stmt_to_string stmt) ; *)

     let typ_to_bytes = function Reg bits -> (bits+7)/8 | _ -> failwith "not a register" in
     let get_byte i v = Int64.logand (Int64.shift_right v ((i-1)*8)) 0xffL in
     let rec add_mem index value = function
      | 0 -> ()
      | n -> let byte = get_byte n value in
             Hashtbl.add concrete_mem index (Int(byte,reg_8)) ;
             add_mem (Int64.succ index) value (n-1)
     in
     let add_to_conc {name=name; mem=mem; index=index; value=value; t=typ} =
       if mem then
        add_mem index value (typ_to_bytes typ)
       else
        Hashtbl.add concrete name (Int(value,typ))
     in
     (match stmt with
       | Label (_,atts) -> 
         let conc_atts = List.filter (function Context _ -> true | _ -> false) atts in
         let conc_atts = List.map (function Context c -> c | _ -> def) conc_atts in
         if conc_atts != [] then (Hashtbl.clear concrete;Hashtbl.clear concrete_mem);
         List.iter add_to_conc conc_atts
       | _ -> ()
     );
     Trace.eval_stmt state stmt
    with Failure fail -> 
      (try
      let prev = Trace.inst_fetch state.sigma (Int64.pred state.pc) in
      failwith (fail^"\nprev: "^(Pp.ast_stmt_to_string prev))
      with Failure _ -> failwith (fail^" no prev"))
  

