(* A module to perform AST-evaluation *)

  open Ast
  open Type

(* Some useful types *)
  type addr = int64
  type instr = stmt
  type varid = Ast.exp
  type varval = Ast.exp (* Int of int | Mem of mem | Str of string | Unit *)
  type label_kind = label

(* Lookup functions for basic contexts *)
  let inst_fetch sigma pc =
    try Hashtbl.find sigma pc 
    with Not_found -> failwith "jump out of range (pc not in dom(Sigma))"

  let label_decode lambda lab =
    try Hashtbl.find lambda lab
    with Not_found -> failwith "jump to inexistent label"

  let lookup_var delta var =
    try Hashtbl.find delta var
    with Not_found -> failwith ("uninitialized variable "^(Var.name var))

  let lookup_mem mu index =
    try Hashtbl.find mu index 
    with Not_found -> failwith "uninitialized memory"

(* Context update *)
  let context_update = Hashtbl.replace
  let context_copy = Hashtbl.copy

(* Context Contents *)

  let print_values delta =
    Printf.printf "values:\n" ;
    Hashtbl.iter 
      (fun k v ->
        match k,v with 
          | var,Int (n,_) -> Printf.printf "%s = %s\n" (Var.name var) (Int64.to_string n)
          | _ -> ()
      ) delta

  let print_mem memory =
    Printf.printf "memory:\n" ;
    Hashtbl.iter 
      (fun k v ->
        match k,v with 
          | Int (v,_),Int (n,_) -> Printf.printf "%s -> %s\n" (Int64.to_string v) (Int64.to_string n)
          | _ -> ()
      ) memory


  (* Evaluate an expression in a context Delta,Mu *)
  let rec eval_expr (delta,mu,expr) =
    let get_expr e = 
          match eval_expr (delta,mu,e) with
           | Int (v,t) -> (v,t)
           | _ -> failwith "expression cannot be evaluated"
    in
    let eval = function 
      | Var v -> 
        lookup_var delta v
      | Int _ as value -> 
        value
      | Lab _ as labl ->
        labl
      | BinOp (op,e1,e2) ->
        let v1 = get_expr e1 
        and v2 = get_expr e2 in
        let (v,t) = Arithmetic.binop op v1 v2 in
        Int (v,t)
      | UnOp (op,e) ->
        let v = get_expr e in
        let (v,t) = Arithmetic.unop op v in
        Int (v,t)
      | Let (var,e1,e2) ->
        let v1 = eval_expr (delta,mu,e1) in
        let delta' = context_copy delta in
        context_update delta' var v1 ;
        let v2 = eval_expr (delta',mu,e2) in
        v2
      | Load (e1,e2,e3,t) ->
        let v1 = eval_expr (delta,mu,e1) 
        and v2 = eval_expr (delta,mu,e2)
        and v3 = eval_expr (delta,mu,e3) in
        let arr = BinOp (PLUS,v1,v2) in
        (match t with
          | Array _ -> (* LOAD_array *)
            failwith "loading array currently unsupported"
          | Reg bits -> (* Load to register *)
            let n = bits/8 in (* FIXME: 1-bit loads? *)
            (* loading the bytes *)
            let rec get_bytes offset acc =
              if offset = n then acc
              else 
                let mem_index = BinOp (PLUS,arr,Int(Int64.of_int offset, Reg 64)) in
                let index = eval_expr (delta,mu,mem_index) in
                let byte = lookup_mem mu index in
                get_bytes (offset+1) (byte::acc)
            in
            (* changing the order according to the endianness *)
            let loaded = 
              let bytes = get_bytes 0 [] in
              if v3 = exp_false then bytes else List.rev bytes
            and byte_size = Int(8L,Reg 64) in
            (* calculating the loaded value *)
            let value = 
              List.fold_left
                (fun v n ->
                  let shl = (BinOp(LSHIFT,v,byte_size)) in
                  BinOp(OR,shl,n)
                ) (Int(0L,Reg 64)) loaded 
            in
            value
          | _ -> failwith "not a loadable type"
        )
      | Store (e1,e2,e3,e4,t) ->
        let v1 = eval_expr (delta,mu,e1) 
        and v2 = eval_expr (delta,mu,e2) 
        and v3 = eval_expr (delta,mu,e3) in
        let arr = BinOp (PLUS,v1,v2) in
        (match t with
          | Array _ -> (* STORE_array *)
            failwith "storing array currently unsupported"
          | Reg bits -> 
            let v4 = eval_expr (delta,mu,e4) in
            let n = bits/8 in (* FIXME: 1-bit stores? *)
            let lsb = 0xffffL in
            (* Break the value down to bytes *)
            let rec get_bytes offset (v,pos,vals) =
              if offset = n then (v,pos,vals)
              else 
                let index = BinOp (PLUS,arr,Int(Int64.of_int offset, Reg 64)) in
                let ind = eval_expr (delta,mu,index) in
                let byte = BinOp (AND,v,Int(lsb,Reg 64)) in
                let ebyte = eval_expr (delta,mu,byte) in
                let v' = (BinOp (RSHIFT,v,Int(8L,Reg 64))) in
                get_bytes (offset+1) (v',ind::pos,ebyte::vals)
            in
            let _,poss,vals = get_bytes 0 (v3,[],[]) in
            (* Changing the indices based on endianness *)
            let poss = if v4 = exp_false then poss else List.rev poss in
            List.iter2 (fun pos value -> context_update mu pos value) poss vals ;
            v3
          | _ -> failwith "not a storable type"
        ) 
      | Cast _ 
      | Unknown _ -> exp_false
    in 
    eval expr


(* The statement evaluation is practically a transition: *
 * (Delta,Mu,pc,stmt) -> (Delta',Mu',pc',stmt')          *
 * The contexts Sigma, Lambda remain unchanged during    *
 * transitions, but that can be easily changed           *)
  let rec eval_stmt (delta,mu,sigma,lambda,pc,stmt,halt) = 
    let get_label e =
      let v = eval_expr (delta,mu,e) in
      match lab_of_exp v with
             | None -> failwith "not a valid label"
             | Some lab -> label_decode lambda lab
    in
    let eval = function
      | Move (v,e,_) ->
         let ev = eval_expr (delta,mu,e) in
         context_update delta v ev ;
         let pc' = Int64.succ pc in
         (delta,mu,pc',halt)
      | Halt (_, _) -> 
      (* we don't care about the return value for the time being *)
             (delta,mu,pc,true)
      | Jmp (e,_) -> 
             let pc' = get_label e in
             (delta,mu,pc',halt) ;
      | CJmp (b,e1,e2,_) ->
          let pc' = match eval_expr (delta,mu,b) with
            | v when v = exp_true -> get_label e1
            | v when v = exp_false -> get_label e2
            | _ -> failwith "not a boolean condition"
          in
            (delta,mu,pc',halt)
      | Assert (e,_) ->
        let v = eval_expr (delta,mu,e) in
        if v = exp_false then failwith ("assertion failed "^(Int64.to_string pc))
        else (delta,mu,Int64.succ pc,halt)
      | _ -> (delta,mu,Int64.succ pc,halt)
    in
    let (delta',mu',pc',halt) as c = eval stmt in
     if halt then c
     else 
      let stmt' = inst_fetch sigma pc' in
        eval_stmt (delta',mu',sigma,lambda,pc',stmt',halt)

(* Symbol meanings:                              *
 * pc: the program counter                       *
 * Sigma: mapping the program counter to a stmt  *
 * Lambda: mapping a label to a program counter  *
 * Delta: mapping a variable to a value          *
 * Mu: mapping a memory index to a value         *)
  let eval_ast_program stmts =
    let sigma : (addr, instr) Hashtbl.t = Hashtbl.create 5700 
    and pc = Int64.zero 
    and delta : (Ast.var, varval) Hashtbl.t = Hashtbl.create 5700
    and mu : (Ast.exp, Ast.exp) Hashtbl.t = Hashtbl.create 5700
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
    ignore (eval_stmt (delta,mu,sigma,lambda,pc,init,false)) ;
    print_values delta ;
    print_mem mu

