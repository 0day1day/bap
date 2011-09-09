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
open Big_int
open Big_int_convenience
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
exception UnknownLabel of label_kind

(* An assertion failed *)
exception AssertFailed of ctx

(* Memory equality

   XXX: This should be in arithmetic.ml, but the evaluator uses an
   internal type AddrMap to represent concrete memories.

   XXX: The semantics of memory equality are undefined.  For now,
   we'll say that an initial memory is undefined. So, unless a
   concrete memory is completely specified (e.g., has values for all
   2^32 mappings), it will not equal another concrete memory unless
   they originated from the same variable. *)
let eq_concrete_mem (m1,v1) (m2,v2) =
  let bool_to_astint = function
    | true -> (bi1, reg_1)
    | false -> (bi0, reg_1)
  in
  (* let print_map m = dprintf "Printing map"; AddrMap.iter (fun k m -> dprintf "%Lx -> %s" k (Pp.ast_exp_to_string m)) m in *)
  (* dprintf "cmpmem: %s %s" (Var.name v1) (Var.name v2); *)
  (* print_map m1; print_map m2; *)
  if v1 <> v2 then bool_to_astint false (* XXX: Technically, we should check to make sure that both
                                           mappings are not completely full and equivalent. *)
  else bool_to_astint (AddrMap.equal (===) m1 m2)

let mem_binop op m1 m2 = match op with
  | EQ -> eq_concrete_mem m1 m2
  | NEQ -> Arithmetic.unop NOT (eq_concrete_mem m1 m2)
  | _ -> failwith ((Pp.binop_to_string op) ^ " undefined for memories")

(* Useful shorthands *)
let empty_mem v = ConcreteMem(AddrMap.empty, v)
let empty_smem v = Symbolic(Var(v))
let val_true = Symbolic exp_true
let val_false = Symbolic exp_false
let is_true_val = function
  | Symbolic e -> full_exp_eq e exp_true
  | ConcreteMem _ -> false
let is_false_val = function
  | Symbolic e -> full_exp_eq e exp_false
  | ConcreteMem _ -> false
let is_symbolic = function
  | Symbolic (Int _) -> false
  | ConcreteMem _ -> false
  | _ -> true
let is_concrete = function
  | Int _ -> true
  | _ -> false
let is_concrete_mem = function
  | ConcreteMem _ -> true
  | Symbolic _ -> false
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
let concmem_to_mem = function
  | ConcreteMem m -> m
  | _ -> failwith "not a concrete memory"
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
  
module type EvalTune =
sig
  val eval_symb_let : bool 
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
  
module Make(MemL: MemLookup)(Tune: EvalTune)(Form: Formula) = 
struct
  
  let byte_type = reg_8
  let index_type = reg_32

  (* Lookup functions for basic contexts *)
  let inst_fetch sigma pc =
    Hashtbl.find sigma pc 
      
  let label_decode lambda lab =
    try Hashtbl.find lambda lab
    with Not_found -> 
      match lab with
	| Name _ (*-> failwith ("jump to inexistent label "^s)*)
	| Addr _ -> 
	    (* I'd like to print a warning here, but traces rely on this
	       behavior, so it prints a lot of warnings if we leave it
	       on. *)
	    (* wprintf "Unknown label: %s" (Pp.label_to_string lab); *)
	    raise (UnknownLabel lab)(*failwith ("jump to inexistent label "^
					 (Printf.sprintf "%Lx" x)) *)

  let lookup_var        = MemL.lookup_var
  let update_mem        = MemL.update_mem
  let lookup_mem        = MemL.lookup_mem
  let assign            = MemL.assign
  let eval_symb_let     = Tune.eval_symb_let    
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

  (** Number of variable locations stored in state *)
  let num_values delta =
    VH.length delta

  (** Number of concrete memory locations stored in state *)
  let num_mem_locs delta =
    (** Number of bindings in map

	XXX: This is inefficient; switch to BatMaps which support cardinality
    *)
    let map_length m =
      AddrMap.fold (fun _ _ c -> c+1) m 0
    in
    VH.fold
      (fun k v count  ->
	 match k,v with
	   | var, ConcreteMem(mem,_) ->
             count + (map_length mem)
	   | _ -> count
      ) delta 0
      

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
    (* Evaluate an expression e, but only if the evaluation will be
       the same size as the unevaluated form (or smaller). Each
       expression type can assume that subexpression evaluation has
       the same property. *)
    let eval = function 
      | Var v -> 
	  (* This can clearly result in a large symbolic
	     result. Should we leave Vars? *)
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
      | Extract(h,l,e) ->
	  let v = eval_expr delta e in
	  if is_symbolic v then (
	    Symbolic(Extract(h,l,symb_to_exp v))
	  ) else (
	    let (v,t) = Arithmetic.extract h l (concrete_val_tuple v) in
	    Symbolic(Int(v,t))
	  )
      | Concat(le,re) ->
	  let lv = eval_expr delta le
	  and rv = eval_expr delta re in
	  if is_symbolic lv || is_symbolic rv then (
	    Symbolic(Concat(symb_to_exp lv, symb_to_exp rv))
	  ) else (
	    let lv = concrete_val_tuple lv in
	    let rv = concrete_val_tuple rv in
	    let (v,t) = Arithmetic.concat lv rv in
	    Symbolic(Int(v,t))
	  )
      | BinOp (op,e1,e2) ->
	  (* In the worst case, we will just combine two symbolic
	     expressions. *)
	  let v1 = eval_expr delta e1
	  and v2 = eval_expr delta e2 in
          (match v1, v2 with
          | Symbolic (Int e1'), Symbolic (Int e2') ->
              (* We have two concrete scalars *)
	      let (v,t) = (Arithmetic.binop op e1' e2') in
	      Symbolic (Int(v,t))
          | ConcreteMem m1, ConcreteMem m2 ->
              (* We have two concrete memories. Note that this can
                 only return bool. *)
              Symbolic (Int (mem_binop op m1 m2))
          | _ ->
              (* Something is symbolic *)
	      let e1' = symb_to_exp v1
	      and e2' = symb_to_exp v2 in
	      Symbolic (BinOp (op, e1', e2')))
      | UnOp (op,e) ->
	  (* In the worst case, we will have a symbolic expression. *)
	  let v = eval_expr delta e in
	    if is_symbolic v then 
	      Symbolic(UnOp(op, symb_to_exp v))
	    else
	      let e' = concrete_val_tuple v in
	      let (v,t) = Arithmetic.unop op e' in
		Symbolic (Int (v,t))
      | Cast (ct,t,e) -> 
	  (* In the worst case, we will have a symbolic expression. *)
	  let v = eval_expr delta e in
	    if is_symbolic v then
	      let e' = symb_to_exp v in
		Symbolic(Cast(ct, t, e'))
	    else 
	      let e' = concrete_val_tuple v in
	      let (n',t') = Arithmetic.cast ct e' t in
		Symbolic (Int (n',t'))
      | Let (var,e1,e2) as l ->
	  (* Consider let v=e in e+e+e+e+e+e+e+e+e+e+e+e+e+e+e. If e
	     is not concrete, this could lead to a huge blowup.

	     So, if e is symbolic, we won't attempt to evaluate the expression
	     further at all. *)
	  let v1 = eval_expr delta e1 in
	  if is_symbolic v1 && not eval_symb_let then
	    Symbolic(l)
	  else
	    let delta' = context_copy delta in (* FIXME: avoid copying *)
	    context_update delta' var v1 ;
	    let v2 = eval_expr delta' e2 in
	    (* So, this is a little subtle.  Consider what happens if
	       we have let x = 1 in let foo = freevar in x. We would
	       evaluate let foo = freevar in x in the context where x
	       is mapped to 1.  However, since freevar is a symbolic
	       expression, we would not evaluate it further, and would
	       return the evaluation expression let foo = freevar in
	       x.  However, this is incorrect, because we are removing
	       the Let binding for x!  We should really wrap any free
	       variable with a Let binding to its current value in the
	       context.

	       Unfortunately, the way that lookup_var is implemented
	       does not make it easy to know whether a variable is
	       really defined or not.  (In traces, we return 0
	       whenever we see an unknown variable, for instance.) So,
	       as a stopgap measure, if var is free in v2, we return
	       the original expression. *)
	    (match v2 with
	    | Symbolic v2' ->
	      let fvars = Formulap.freevars v2' in
	      let isvar = (fun v -> not (Var.equal v var)) in
	      if List.for_all isvar fvars then
		(* var is not free! We are good to go *)
	      v2
	      else
		(* var is still free; we can't use the evaluated version *)
		Symbolic(l)
	    | _ -> v2)
      | Load (mem,ind,endian,t) ->
	(match t with
	| Reg 8 ->
		 (* This doesn't introduce any blowup on its own. *)
		 let mem = eval_expr delta mem 
		 and ind = eval_expr delta ind
		 and endian = eval_expr delta endian in
		 let mem_arr = symb_to_exp ind 
		 and endian_exp = symb_to_exp endian in
		   Symbolic (lookup_mem mem mem_arr endian_exp)
	     | Reg _ ->  (* we only care about 32bit *)
		 (* Splitting introduces blowup.  Can we avoid it? *)
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
		   (* No blowup here. *)
		   let mem = eval_expr delta mem in
		     update_mem mem index value endian
	       | Reg _ -> 
		   (* Splitting blowup, but I don't know how to avoid this. *)
		   eval_expr delta 
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

  (** Evaluate as long as there is exactly one choice of state.

      @param step This function is called with the evaluator's state
      for each transition.

  *)
  let eval_straightline ?(step = Util.id) state =
    let rec f state =
      match eval state with
      | newstate::[] -> f (step newstate)
      | states -> states
    in
    f state
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
		  (fun k v m -> Store (m,Int(big_int_of_int64 k,reg_32),v,exp_false,reg_8))
		  memory init)
	
  (* Normalize a memory address, setting high bits to 0. *)
  let normalize i t = int64_of_big_int (Arithmetic.to_big_int (i,t))
    
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
	    Symbolic(Int(bi0, t))
	    
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
	     Int(bi0, reg_8)
	  )
      | _ -> failwith "Symbolic memory or address in concrete evaluation"
	  
  let assign v ev ({delta=delta; pred=pred; pc=pc} as ctx) =
    context_update delta v ev ;
    [{ctx with pc=Int64.succ pc}]
end

module FastEval =
struct
  let eval_symb_let = false
end

module SlowEval =
struct
  let eval_symb_let = true
end

module StdForm =
struct
  let add_to_formula formula expression _type =
    BinOp(AND, expression, formula)
      
  let output_formula () = failwith "Formula output unsupported"
end
  
module Symbolic = Make(SymbolicMemL)(FastEval)(StdForm)
module SymbolicSlow = Make(SymbolicMemL)(SlowEval)(StdForm)

module Concrete = Make(ConcreteMemL)(FastEval)(StdForm)

(** Execute a program concretely *)
let concretely_execute ?s ?(i=[]) p =
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
  let ctx = match s with
    | Some(s) -> {ctx with pc = Concrete.label_decode ctx.lambda (Addr s)} 
    | None -> ctx
  in
  let ctx = step ctx in
  Concrete.print_values ctx.delta;
  Concrete.print_mem ctx.delta;
  ctx

let eval_expr = Symbolic.eval_expr

