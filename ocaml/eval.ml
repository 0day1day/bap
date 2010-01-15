(** A module to perform AST-evaluation *)

(** Things that you should be aware before using it:
 *
 *  - All programs should end with a halt 'value'
 *    (not having a halt will result in an error message 
 *
 *  - The LOAD-table and STORE-table abilities are not
 *    implemented yet. `Specials' are also not handled yet.
 *
 *  - The behaviour of the evaluator should be faithful to
 *    the operational semantics of BIL (as provided by the 
 *    BAP Handbook.
 *)

(** TODO list:
 *   - Test it on several IL programs
 *   - Add feature for zero default values of unbound vars
 *   - Add feature to provide values of unbound vars
 *   - Cleanup Load-Store to make more readable
 *)

  open Ast
  open Type

(* Some useful types *)
  type addr = int64
  type instr = stmt
  type varid = Ast.exp
  type varval = Val of Ast.exp | Mem of (Ast.exp, Ast.exp) Hashtbl.t
  type label_kind = label

  exception ExcState of string * addr

(* Context update *)
  let context_update = Hashtbl.replace
  let context_copy = Hashtbl.copy

(* Lookup functions for basic contexts *)
  let inst_fetch sigma pc =
    try Hashtbl.find sigma pc 
    with Not_found -> failwith "jump out of range (pc not in dom(Sigma))"

  let label_decode lambda lab =
    try Hashtbl.find lambda lab
    with Not_found -> failwith "jump to inexistent label"

  let lookup_var delta var =
    try Hashtbl.find delta var
    with Not_found -> 
      match Var.typ var with
        | TMem _ -> 
          let newmemory = Mem (Hashtbl.create 1000) in
          context_update delta var newmemory ;
          newmemory
        | _ -> failwith ("uninitialized variable "^(Var.name var))

  let lookup_mem mu index =
    try Hashtbl.find mu index 
    with Not_found -> failwith "uninitialized memory"

(* Wrapping - Unwrapping functions *)

  let wrap_val v = Val v
  let unwrap_val = function 
    | Val v -> v
    | Mem _ -> failwith "unwrapping a memory, not a value"

  let wrap_mem m = Mem m
  let unwrap_mem = function
    | Mem m -> m
    | Val _ -> failwith "unwrapping a value, not a memory"

(* Context Contents *)

  let print_values delta =
    Printf.printf "contents of variables:\n" ;
    Hashtbl.iter 
      (fun k v ->
        match k,v with 
          | var,Val (Int (n,_)) -> Printf.printf "%s = %s\n" (Var.name var) (Int64.to_string n)
          | _ -> ()
      ) delta

  let print_mem memory =
    Printf.printf "contents of memories:\n" ;
    Hashtbl.iter 
      (fun k v ->
        match k,v with 
          | var,Mem mem -> 
             Printf.printf "memory %s:\n" (Var.name var) ; 
             Hashtbl.iter
               (fun k1 v1 -> 
                 match k1,v1 with
                  | Int (n1,_), Int (n2,_) ->
                    Printf.printf "%s -> %s\n" (Int64.to_string n1) (Int64.to_string n2)
                  | _ -> ()
               ) mem
          | _ -> ()
      ) memory


  (* Evaluate an expression in a context Delta,Mu *)
  let rec eval_expr (delta,expr) : varval =
    let get_expr e = 
          match eval_expr (delta,e) with
           | Val (Int (v,t)) -> (v,t)
           | Val v -> failwith ("expression cannot be evaluated:\n"^(Pp.ast_exp_to_string v))
           | Mem _ -> failwith "tried to perform memory operations"
    in
    let eval = function 
      | Var v -> 
        lookup_var delta v
      | Int _ as value -> 
        Val value
      | Lab _ as labl ->
        Val labl
      | BinOp (op,e1,e2) ->
        let v1 = get_expr e1 
        and v2 = get_expr e2 in
        let (v,t) = Arithmetic.binop op v1 v2 in
        Val (Int (v,t))
      | UnOp (op,e) ->
        let v = get_expr e in
        let (v,t) = Arithmetic.unop op v in
        Val (Int (v,t))
      | Cast (ct,t,e)  -> 
        let (n,t2) = get_expr e in
        let (n',t') = Arithmetic.cast ct (n,t2) t in
         Val (Int (n',t'))
      | Let (var,e1,e2) ->
        let v1 = eval_expr (delta,e1) in
        let delta' = context_copy delta in
        context_update delta' var v1 ;
        let v2 = eval_expr (delta',e2) in
        v2
      | Load (e1,e2,e3,t) ->
        let v1 = eval_expr (delta,e1) 
        and v2 = eval_expr (delta,e2)
        and v3 = eval_expr (delta,e3) in
        (match t with
          | Array _ -> (* LOAD_array *)
            failwith "loading array currently unsupported"
          | Reg bits -> (* Load to register *)
            let n = bits/8 in (* FIXME: 1-bit loads? *)
            (* loading the bytes *)
            let memory = unwrap_mem v1
            and start_index = unwrap_val v2 in
            let rec get_bytes offset acc =
              if offset = n then acc
              else 
                let mem_index = BinOp (PLUS,start_index,Int(Int64.of_int offset, Reg 64)) in
                let index = unwrap_val (eval_expr (delta,mem_index)) in
                let byte = lookup_mem memory index in
                get_bytes (offset+1) (byte::acc)
            in
            (* changing the order according to the endianness *)
            let loaded = 
              let bytes = get_bytes 0 [] in
              if v3 = Val exp_false then bytes else List.rev bytes
            and byte_size = Int(8L,Reg 64) in
            (* calculating the loaded value *)
            let value = 
              List.fold_left
                (fun v n ->
                  let shl = (BinOp(LSHIFT,v,byte_size)) in
                  unwrap_val (eval_expr (delta,BinOp(OR,shl,n)))
                ) (Int(0L,Reg bits)) loaded 
            in
            Val value
          | _ -> failwith "not a loadable type"
        )
      | Store (e1,e2,e3,e4,t) ->
        let v1 = eval_expr (delta,e1) 
        and v2 = eval_expr (delta,e2) 
        and v3 = eval_expr (delta,e3) in
        (match t with
          | Array _ -> (* STORE_array *)
            failwith "storing array currently unsupported"
          | Reg bits -> 
            let memory = unwrap_mem v1
            and start_index = unwrap_val v2 
            and v4 = eval_expr (delta,e4) 
            and n = bits/8  (* FIXME: 1-bit stores? *)
            and lsb = 0xffL in
            (* Break the value down to bytes *)
            let rec get_bytes offset (v,pos,vals) =
              if offset = n then (v,pos,vals)
              else 
                let index = BinOp (PLUS,start_index,Int(Int64.of_int offset, Reg 64)) in
                let ind = unwrap_val (eval_expr (delta,index)) in
                let byte = BinOp (AND,v,Int(lsb,Reg 64)) in
                let ebyte = unwrap_val (eval_expr (delta,byte)) in
                let v' = (BinOp (RSHIFT,v,Int(8L,Reg 64))) in
                get_bytes (offset+1) (v',ind::pos,ebyte::vals)
            in
            let _,poss,vals = get_bytes 0 (unwrap_val v3,[],[]) in
            (* Changing the indices based on endianness *)
            let poss = if v4 = Val exp_false then poss else List.rev poss in
            List.iter2 (fun pos value -> context_update memory pos value) poss vals ;
            Mem memory
          | _ -> failwith "not a storable type"
        ) 
      | Unknown _ -> Val exp_false
    in 
      eval expr


(* The statement evaluation is practically a transition: *
 * (Delta,Mu,pc,stmt) -> (Delta',Mu',pc',stmt')          *
 * The contexts Sigma, Lambda remain unchanged during    *
 * transitions, but that can be easily changed           *)
  let rec eval_stmt (delta,sigma,lambda,pc,stmt,halt) = 
    let get_label e =
      let v = eval_expr (delta,e) in
      match lab_of_exp (unwrap_val v) with
             | None -> failwith "not a valid label"
             | Some lab -> label_decode lambda lab
    in
    let eval = function
      | Move (v,e,_) ->
         let ev = eval_expr (delta,e) in
         context_update delta v ev ;
         let pc' = Int64.succ pc in
         (delta,pc',halt)
      | Halt (_, _) -> 
      (* we don't care about the return value for the time being *)
             (delta,pc,true)
      | Jmp (e,_) -> 
             let pc' = get_label e in
             (delta,pc',halt) ;
      | CJmp (b,e1,e2,_) ->
          let pc' = match eval_expr (delta,b) with
            | Val v when v = exp_true -> get_label e1
            | Val v when v = exp_false -> get_label e2
            | _ -> failwith "not a boolean condition"
          in
            (delta,pc',halt)
      | Assert (e,_) ->
        let v = eval_expr (delta,e) in
        if v = Val exp_false then failwith ("assertion failed "^(Int64.to_string pc))
        else (delta,Int64.succ pc,halt)
      | Comment _ -> (delta,Int64.succ pc,halt)
      | Special _ -> failwith "Specials not handled yet!"
    in
    let (delta',pc',halt) as c = 
      try eval stmt with Failure str -> raise (ExcState (str, pc))
    in
     if halt then c
     else 
      let stmt' = inst_fetch sigma pc' in
        eval_stmt (delta',sigma,lambda,pc',stmt',halt)

(* Symbol meanings:                              *
 * pc: the program counter                       *
 * Sigma: mapping the program counter to a stmt  *
 * Lambda: mapping a label to a program counter  *
 * Delta: mapping a variable to a value          *)
  let eval_ast_program stmts =
    let sigma : (addr, instr) Hashtbl.t = Hashtbl.create 5700 
    and pc = Int64.zero 
    and delta : (Ast.var, varval) Hashtbl.t = Hashtbl.create 5700
    and lambda : (label_kind, addr) Hashtbl.t = Hashtbl.create 5700 in
    (* Initializing Sigma and Lambda *)
    ignore 
      (List.fold_left
        (fun pc s ->
          Hashtbl.add sigma pc s ;
          (match s with
            | Label (lab,_) ->
              Hashtbl.add lambda lab pc
            | _ -> () 
          ) ;
          Int64.succ pc
        ) pc stmts ) ; 
    let init = inst_fetch sigma pc in
    (try ignore (eval_stmt (delta,sigma,lambda,pc,init,false)) 
     with ExcState (s, pc) -> 
      (prerr_endline ("Evaluation aborted at line "^(Int64.to_string pc));
       prerr_endline ("reason: "^s))
    );
    print_values delta;
    print_mem delta

