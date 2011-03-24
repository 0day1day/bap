(** A module to perform AST Symbolic Execution *)

(** TODO list:
 *
 *  - Cleanup & make readable
 *
 *  - Still experimental -- needs testing
 *
 *  - Need to figure out a way to show uninitialized memories
 *    and variables (because right now Unknown + name is used)
 *  
 *  - We need to figure out what we are going to do with
 *    symbolic memory accesses.
 *
 *  - Some sort of mapping from variables to instance variables.
 *)

open Ast
open Type
  
module VH = Var.VarHash
  
module D = Debug.Make(struct let name = "SymbEval" and default=`Debug end)
open D

(* For now, we'll map every byte. Later it may be better to map larger
   values, but we'd lose precision wrt uninitialized memory. *)
module AddrMap = Map.Make(Int64)
  
type mem = Ast.exp AddrMap.t * Var.t (* addr -> val + initial name *)
    
(* Some useful types *)
type addr = int64
type instr = stmt
type varid = Ast.exp
type varval = Symbolic of Ast.exp | ConcreteMem of mem
type label_kind = label
type form_type = Equal | Rename
    
(* Evaluator Contexts *)
(* Symbol meanings:                              *
 * pc: the program counter                       *
 * Sigma: mapping the program counter to a stmt  *
 * Lambda: mapping a label to a program counter  *
 * Delta: mapping a variable to a value          *
 * Pred: the path predicate                      *)
type ctx = {
  pred: Ast.exp;
  delta: varval VH.t;
  sigma: (addr, instr) Hashtbl.t;
  lambda: (label_kind, addr) Hashtbl.t;
  pc: addr; (* Should be int *)
}
    
(* Exceptions *)
exception ExcState of string * addr
  
(* Program halted, with optional halt value, and with given execution context. *)
exception Halted of varval option * ctx

(* An unknown label was found *)  
exception UnknownLabel

(* An assertion failed *)
exception AssertFailed of ctx

(* Useful shorthands *)
let empty_mem v = ConcreteMem(AddrMap.empty, v)
let empty_smem v = Symbolic(Var(v))
let val_true = Symbolic exp_true
let val_false = Symbolic exp_false
let is_true_val = (=) val_true
let is_false_val = (=) val_false
let is_symbolic = function
  | Symbolic (Int _) -> false
  | _ -> true
let is_concrete = function
  | Int _ -> true
  | _ -> false
let concrete_val_tuple = function
  | Symbolic (Int (v,t)) -> (v,t)
  | Symbolic e -> 
      failwith ("expression cannot be evaluated concretely:\n"
		^(Pp.ast_exp_to_string e))
  | _ -> failwith "tried to perform memory operations"
      
(* Context functions (lookup, update etc) *)
let context_update = VH.replace
let context_copy = VH.copy
  
(* Unwrapping functions *)
let symb_to_exp = function
  | Symbolic e -> e
  | ConcreteMem _ -> failwith "this is not a symbolic expression"
      
let symb_to_string = function
  | Symbolic e -> "Symbolic "^(Pp.ast_exp_to_string e)
  | ConcreteMem m -> "Memory"
      
module type MemLookup =
sig
  val lookup_var : (varval VH.t) -> VH.key -> varval
  val update_mem : varval -> Ast.exp -> Ast.exp -> Ast.exp -> varval
  val lookup_mem : varval -> Ast.exp -> Ast.exp -> Ast.exp
  val assign : Var.t -> varval -> ctx -> ctx list
end
  
module type SymbMem =
sig
  val is_symbolic_store : Ast.exp -> bool 
end

(* A new interface for the result handling *)
module type Result =
sig
  val success : ctx -> 'a
  val failed_assert : ctx -> 'a
  val failure : string -> 'a
end

module type Formula =
sig
  val add_to_formula : Ast.exp -> Ast.exp -> form_type -> Ast.exp
    (* FIXME *)
  val output_formula : unit -> Ast.exp
end
  
module Make(MemL: MemLookup)(SMem: SymbMem)(Form: Formula) = 
struct
  
  let byte_type = reg_8
  let index_type = reg_32
    
  let is_symbolic_store = SMem.is_symbolic_store
    
  (* Lookup functions for basic contexts *)
  let inst_fetch sigma pc =
    Hashtbl.find sigma pc 
      
  let label_decode lambda lab =
    try Hashtbl.find lambda lab
    with Not_found -> 
      match lab with
	| Name _ (*-> failwith ("jump to inexistent label "^s)*)
	| Addr _ -> 
	    wprintf "Unknown label: %s" (Pp.label_to_string lab);
	    raise UnknownLabel (*failwith ("jump to inexistent label "^
					 (Printf.sprintf "%Lx" x)) *)
	    
  let lookup_var        = MemL.lookup_var
  let update_mem        = MemL.update_mem
  let lookup_mem        = MemL.lookup_mem
  let assign            = MemL.assign
  let is_symbolic_store = SMem.is_symbolic_store
  let add_constraint    = Form.add_to_formula
  let output_formula    = Form.output_formula
    
  (* Initializers *)
  let create_state () = 
    let sigma : (addr, instr) Hashtbl.t = Hashtbl.create 5700 
    and pc = Int64.zero 
    and delta : varval VH.t = VH.create 5700
    and lambda : (label_kind, addr) Hashtbl.t = Hashtbl.create 5700 in
      {pred=exp_true; delta=delta; sigma=sigma; lambda=lambda; pc=pc} 
	
  let initialize_prog state prog_stmts =
    Hashtbl.clear state.sigma ;
    Hashtbl.clear state.lambda ;
    (* Initializing Sigma and Lambda *)
    ignore 
      (List.fold_left
         (fun pc s ->
          Hashtbl.add state.sigma pc s ;
            (match s with
               | Label (lab,_) -> Hashtbl.add state.lambda lab pc
               | _ -> () 
            ) ;
            Int64.succ pc
         ) state.pc prog_stmts )
      
  let cleanup_delta state =
    VH.clear state.delta
      
  let build_default_context prog_stmts =
    let state = create_state() in
      initialize_prog state prog_stmts ;
      state
	
	
  (* Printing the contents of Delta *)
  let print_values delta =
    pdebug "contents of variables" ;
    VH.iter 
      (fun k v ->
	 match k,v with 
	   | var,Symbolic e -> 
               pdebug ((Pp.var_to_string var) ^ " = " ^ (Pp.ast_exp_to_string e))
	   | _ -> ()
      ) delta

  let print_mem delta =
    pdebug "contents of memories" ;
    VH.iter 
      (fun k v ->
	 match k,v with 
	   | var, ConcreteMem(mem,_) -> 
               pdebug ("memory " ^ (Var.name var)) ; 
               AddrMap.iter
		 (fun i v -> 
		    pdebug((Printf.sprintf "%Lx" i) 
			   ^ " -> " ^ (Pp.ast_exp_to_string v))
		 )
		 mem
	   | _ -> ()
      ) delta

  let print_var delta name =
    VH.iter
      (fun var exp ->
	 match exp with
	   | Symbolic e ->
	       let varname = Var.name var in
		 if varname = name then
		   pdebug (varname ^ " = " 
			   ^ (Pp.ast_exp_to_string e))
	   | _ -> ()
      ) delta
      
  (* Evaluate an expression in a context Delta,Mu *)
  let rec eval_expr delta expr =
    (* Decide whether or not we should evaluate the memory, which can
       lead to exponential blow-up due to substitution. *)
    let symb_mem mem = 
      if is_symbolic_store mem then 
	(pdebug "symb mem!" ; Symbolic mem)
      else eval_expr delta mem 
    in
    let eval = function 
      | Var v -> 
	  lookup_var delta v 
      | Int _ as value -> 
	  Symbolic value
      | Lab _ as labl ->
	  Symbolic labl
      | Ite(cond,e1,e2) ->
	  let v1 = eval_expr delta e1
	  and v2 = eval_expr delta e2 in
	  (match eval_expr delta cond with
	   | v when is_symbolic v ->
	       Symbolic(Ite(symb_to_exp v, symb_to_exp v1, symb_to_exp v2))
	   | v when is_true_val v ->
	       Symbolic(symb_to_exp v1)
	   | v when is_false_val v ->
	       Symbolic(symb_to_exp v2)
	   | _ ->
	       failwith "not possible"
	  )
      | BinOp (op,e1,e2) ->
	  (*let  t1 = Typecheck.infer_ast ~check:false e1 in
	    let  t2 = Typecheck.infer_ast ~check:false e2 in
	    if op = PLUS then 
	    (if t1 != t2 then 
	    (print_endline ((Pp.ast_exp_to_string e1) ^ " " ^  (Pp.ast_exp_to_string e2)) ;
	    assert false)
	    ) ;*)
	  let v1 = eval_expr delta e1
	  and v2 = eval_expr delta e2 in
	    if is_symbolic v1 || is_symbolic v2 then 
	      let e1' = symb_to_exp v1
	      and e2' = symb_to_exp v2 in
		Symbolic (BinOp (op, e1', e2'))
	    else (* if both concrete exprs -> concrete evaluation *)
	      let e1' = concrete_val_tuple v1
	      and e2' = concrete_val_tuple v2 in
	      let (v,t) = (Arithmetic.binop op e1' e2') in
		Symbolic (Int(v,t))
      | UnOp (op,e) ->
	  let v = eval_expr delta e in
	    if is_symbolic v then 
	      Symbolic(UnOp(op, symb_to_exp v))
	    else
	      let e' = concrete_val_tuple v in
	      let (v,t) = Arithmetic.unop op e' in
		Symbolic (Int (v,t))
      | Cast (ct,t,e) -> 
	  let v = eval_expr delta e in
	    if is_symbolic v then
	      let e' = symb_to_exp v in
		Symbolic(Cast(ct, t, e'))
	    else 
	      let e' = concrete_val_tuple v in
	      let (n',t') = Arithmetic.cast ct e' t in
		Symbolic (Int (n',t'))
      | Let (var,e1,e2) ->
	  let v1 = eval_expr delta e1 in
	  let delta' = context_copy delta in (* FIXME: avoid copying *)
	    context_update delta' var v1 ;
	    let v2 = eval_expr delta' e2 in
	      v2
      | Load (mem,ind,endian,t) ->
	  (match t with
	     | Reg 8 ->
		 let mem = symb_mem mem 
		 and ind = eval_expr delta ind
		 and endian = eval_expr delta endian in
		 let mem_arr = symb_to_exp ind 
		 and endian_exp = symb_to_exp endian in
		   Symbolic (lookup_mem mem mem_arr endian_exp)
	     | Reg _ ->  (* we only care about 32bit *)
		 eval_expr delta (Memory2array.split_loads mem ind t endian)
	     | Array _ ->
		 failwith ("loading array currently unsupported" ^ (Pp.typ_to_string t))
	     | _ -> failwith "not a loadable type"
	  )
      | Store (mem,ind,value,endian,t) ->
	  let index = symb_to_exp (eval_expr delta ind)
	  and value = symb_to_exp (eval_expr delta value)
	  and endian = symb_to_exp (eval_expr delta endian) in
	    (match t with
	       | Reg 8 -> 
		   let mem = symb_mem mem in
		     update_mem mem index value endian
	       | Reg _ -> 
		   if not (is_symbolic_store mem) && is_concrete index
		   then eval_expr delta (* we only care about 32bit *)
		     (Memory2array.split_writes mem index t endian value)
		   else symb_mem 
		     (Memory2array.split_writes mem index t endian value)
	       | Array _ -> 
		   failwith "storing array currently unsupported"
	       | _ -> 
		   failwith "not a storable type"
	    )
      | Unknown _ as u -> Symbolic u (*failwith "unknown value encountered"*)
    in 
      eval expr
	
  (* The statement evaluation is practically a transition: *
   * (Delta,Mu,pc,stmt) -> [(Delta',Mu',pc',stmt')]        *
   * The contexts Sigma, Lambda remain unchanged during    *
   * transitions, but that can be easily modified in case  *
   * we need dynamically-loaded code.                      *)
  let rec eval_stmt ({pred=pred; delta=delta; lambda=lambda; pc=pc} as ctx) stmt = 
    let get_label e =
      let v = eval_expr delta e in
	match lab_of_exp (symb_to_exp v) with
	  | None -> failwith ("not a valid label "^(Pp.ast_exp_to_string (symb_to_exp v)))
	  | Some lab -> label_decode lambda lab
    in
    let next_pc = Int64.succ pc in
    let eval = function
      | Move (v,e,_) -> 
	  let ev = eval_expr delta e in
	    assign v ev ctx
      | Halt (e, _) ->
	  let e = eval_expr delta e in
	    raise (Halted(Some e, ctx))
      | Jmp (e,_) -> 
          [{ctx with pc=get_label e}] ;
      | CJmp (cond,e1,e2,_) ->
          (match eval_expr delta cond with
             | v when is_symbolic v ->
		 (* update the path predicate *)
		 let constr = symb_to_exp v in
		 let pred1' = add_constraint pred constr Equal in
		 let neg_constr = UnOp (NOT,constr) in
		 let pred2' = add_constraint pred neg_constr Equal in
		   (* return two new possible states *)
		   [{ctx with pred=pred1'; pc=get_label e1};
		    {ctx with pred=pred2'; delta=context_copy delta; pc=get_label e2}]
             | v when is_true_val v -> 
		 [{ctx with pc=get_label e1}]
             | v when is_false_val v -> 
		 [{ctx with pc=get_label e2}]
             | _ -> failwith "not a boolean condition"
          )
      | Assert (e,_) ->
          (match eval_expr delta e with
             | v when is_symbolic v -> 
		 let constr = symb_to_exp v in
		 let pred' = add_constraint pred constr Equal in
		   (*pdebug("Adding assertion: " ^ (Pp.ast_exp_to_string pred')) ;*)
		   [{ctx with pred=pred'; pc=next_pc}]
             | v when is_false_val v ->
		 raise (AssertFailed ctx)
             | _ -> [{ctx with pc=next_pc}]
          )
      | Comment _ | Label _ -> 
          [{ctx with pc=next_pc}]
      | Special _ -> 
          failwith "Specials not handled yet!"
    in
      eval stmt 
	
(* Performs one evaluation step on the program and returns *
 * a list of all possible follow-up states.                *)
  let eval state =
    try 
      let stmt = inst_fetch state.sigma state.pc in
	(*pdebug (Pp.ast_stmt_to_string stmt) ; *)
	eval_stmt state stmt
    with Failure str -> 
      (prerr_endline ("Evaluation aborted at stmt No-"
                      ^(Int64.to_string state.pc)
                      ^"\nreason: "^str);
       print_values state.delta;
       print_mem state.delta; 
       print_endline ("Path predicate: "^(Pp.ast_exp_to_string state.pred));
       [])
      | Not_found ->
	  (* The only way inst_fetch would fail is if pc falls off the end, right? *)
	  wprintf "PC not found: %#Lx" state.pc;
	  raise (Halted(None, state))
	    
end
  
module SymbolicMemL = 
struct 
  let lookup_var delta var =
    try VH.find delta var
    with Not_found ->
      match Var.typ var with
	| TMem _ 
	| Array _ ->
	    empty_mem var
	| Reg _ ->
	    Symbolic(Var var)
	    
  (* Converting concrete memory to symbolic *)
  let conc2symb memory v =
    pdebug "Concrete to symbolic" ;
    (* FIXME: a better symbolism for uninitialized memories *)
    let init = Var v in
      pdebug "The point of no return" ;
      Symbolic (AddrMap.fold
		  (fun k v m -> Store (m,Int(k,reg_32),v,exp_false,reg_8))
		  memory init)
	
  (* Normalize a memory address, setting high bits to 0. *)
  let normalize i t = Arithmetic.to64 (i,t)
    
  let rec update_mem mu pos value endian = 
    (*pdebug "Update mem" ;*)
    match mu with
      | Symbolic m -> Symbolic (Store(m,pos,value,endian,reg_8))
      | ConcreteMem (m,v) ->
	  match pos with
	    | Int(p,t) ->
		ConcreteMem(AddrMap.add (normalize p t) value m, v)
	    | _ -> update_mem (conc2symb m v) pos value endian
		
  let rec lookup_mem mu index endian =
    (*pdebug "Lookup mem" ;*)
    match mu, index with
      | ConcreteMem(m,v), Int(i,t) ->
	  (try AddrMap.find (normalize i t) m
	   with Not_found ->
	     Load(Var v, index, endian, reg_8)
	       (* FIXME: handle endian and type? *)
	  )
	    (* perhaps we should introduce a symbolic variable *)
      | Symbolic mem, _ -> Load (mem,index,endian,reg_8)
      | ConcreteMem(m,v),_ -> lookup_mem (conc2symb m v) index endian
	  
  let assign v ev ({delta=delta; pred=pred; pc=pc} as ctx) =
    context_update delta v ev ;
    [{ctx with pc=Int64.succ pc}]
end

module ConcreteMemL = 
struct 
  let lookup_var delta var =
    try VH.find delta var
    with Not_found ->
      match Var.typ var with
	| TMem _ 
	| Array _ ->
	    empty_mem var
	| Reg n as t ->
	    Symbolic(Int(0L, t))
	    
  let normalize = SymbolicMemL.normalize
    
  let rec update_mem mu pos value endian = 
    (*pdebug "Update mem" ;*)
    match mu, pos with
      | ConcreteMem (m,v), Int(p,t) ->
	  ConcreteMem(AddrMap.add (normalize p t) value m, v)
      | _ -> failwith "Symbolic memory in concrete evaluation"
		
  let rec lookup_mem mu index endian =
    (*pdebug "Lookup mem" ;*)
    match mu, index with
      | ConcreteMem(m,v), Int(i,t) ->
	  (try AddrMap.find (normalize i t) m
	   with Not_found ->
	     Int(0L, reg_8)
	  )
      | _ -> failwith "Symbolic memory or address in concrete evaluation"
	  
  let assign v ev ({delta=delta; pred=pred; pc=pc} as ctx) =
    context_update delta v ev ;
    [{ctx with pc=Int64.succ pc}]
end
  
module FullSubst =
struct
  let is_symbolic_store = function
    | Store _ (*| Let _*) -> true
    | _ -> false
end

module PartialSubst =
struct
  let is_symbolic_store = function
    | Store _ | Let _ -> true
    | _ -> false
end

module StdForm =
struct
  let add_to_formula formula expression _type =
    BinOp(AND, expression, formula)
      
  let output_formula () = failwith "Formula output unsupported"
end
  
module Symbolic = Make(SymbolicMemL)(FullSubst)(StdForm)
module SymbolicFast = Make(SymbolicMemL)(PartialSubst)(StdForm)

module Concrete = Make(ConcreteMemL)(FullSubst)(StdForm)

(** Execute a program concretely *)
let concretely_execute p ?(i=[]) s  =
  let rec step ctx =
    try
      let s = Concrete.inst_fetch ctx.sigma ctx.pc in
      dprintf "Executing: %s" (Pp.ast_stmt_to_string s);
      match Concrete.eval ctx with
      | [next] -> step next
      | _ -> failwith "step"
    with Halted _ -> ctx
  in
  let ctx = Concrete.build_default_context p in
  (* Evaluate initialization statements *)
  let ctx = List.fold_left (fun ctx s -> 
			      dprintf "Init %s" (Pp.ast_stmt_to_string s);
			      match Concrete.eval_stmt ctx s with
			      | nctx::[] -> nctx
			      | _ -> failwith "Expected one context"
			   ) ctx i
  in
  let ctx = {ctx with pc = Concrete.label_decode ctx.lambda (Addr s)} in
  let ctx = step ctx in
  Concrete.print_values ctx.delta;
  Concrete.print_mem ctx.delta;
  ctx

let eval_expr = Symbolic.eval_expr

