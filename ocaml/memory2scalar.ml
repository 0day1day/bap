(*
  This module turns memory references into scalars.  Specifically,
  every memory access in the form M[addr] where addr is the same
  definition/variable will be turned into a scalar mem_addr, as long
  as addr can be detected as being a constant expression.

  Terminology: A constant memory store means that the *address* of the
  store can be reliably detected as being constant throughout the
  execution of the program.  This should be true (at least) for stack
  accesses.

  Indirect read behavior: 

  (Default) If there is an indirect (non-constant) read detected, then
  the default behavior is to convert all (constant) memory stores into
  both a memory store and a scalar store.  Non-constant memory stores
  remain as memory operations.

  (IndirectReadsOnlyPointToInitRO) If there is an indirect
  (non-constant) read detected, then in this mode any memory stores
  from -init-ro (e.g., writing .rodata from the binary) and any
  non-constant memory store are done as memory operations.  Constant
  memory stores (not from -init-ro) are done as scalar operations
  only.  This option is useful for code in which the only indirect
  reads access a table in .rodata.

  Indirect store behavior:

  (Default) All indirect stores are assumed to never overwrite a
  scalar.  (So, indirect stores should not overwrite stack slots.)  Of
  course, this means that this module is useless for stack-based
  buffer overflows!

  Additional assumptions:
  
  Memory reads and writes never overlap.  If they do, you should not
  be using scalars!  We do not attempt to detect this in any way.

  Only one endianness is used throughout the program.

  There is only one memory (e.g., the program was raised from binary,
  and converted to SSA form).  Programs written in IL with multiple
  memories will not work.

  There may be other assumptions made here; be careful using this
  module!

  -- Ed Schwartz
*)

(*
  Known bugs: If the same address is requested with different types,
  we screw up.  We should probably add a check for this.
*)

module C = Cfg.SSA
module G = C.G
module D = Debug.Make(struct let name = "memtoscalar" and default=`Debug end)
module VH = Var.VarHash
open D
open Ssa

exception Not_simple_mem;;

type readmode =
  | Default (** Default, as described above *)
  | IndirectROPTIR (** Indirect reads only point to init ro memory *)

let readmode_to_string = function
  | Default -> "Default"
  | IndirectROPTIR -> "Indirect Reads Only Point To InitRO"

(* Fill the hash table with var -> expression mappings *)
let buildbtbl defhash g =
  let defvisitor = object(self)
    inherit Ssa_visitor.nop
      
    method visit_stmt stmt =
      begin
	match stmt with 
	| Ssa.Move(lv, e, a) -> begin
	    VH.add defhash lv e; 
(* 	    dprintf "Refdef: We are adding %s\n" (Pp.var_to_string lv) *)
	  end
	| _ -> ()
      end;
      `DoChildren
  end
  in
  G.iter_vertex 
    (fun eachbb ->
       List.iter 
	 (fun stmt -> 
	    ignore(Ssa_visitor.stmt_accept defvisitor stmt))
	 (C.get_stmts g eachbb)) 
    g

(* Use a hash table built with buildbtbl to perform backwards copy
   propagation, e.g., build a single expression *)
let backprop defhash ssaval =
  
  let rec ssaexptoastexp = function
    | Ssa.Load(mem,addr,endian,t) -> Ast.Load((ssavaltoastexp mem), (ssavaltoastexp addr), (ssavaltoastexp endian),t)
    | Ssa.BinOp(binopt,v1,v2) -> Ast.BinOp(binopt, (ssavaltoastexp v1), (ssavaltoastexp v2))
    | Ssa.Cast(ct,t,v) -> Ast.Cast(ct, t, (ssavaltoastexp v)) 
    | Ssa.Val(v) -> ssavaltoastexp v
    | _  as _other -> 
(* 	dprintf "Bad expr: %s" (Pp.ssa_exp_to_string other);  *)
	raise Not_simple_mem;
  and ssavaltoastexp = function
    | Ssa.Var(v) ->
	begin
(* 	  dprintf "Backprop for %s" (Pp.var_to_string v); *)
	  try ssaexptoastexp (VH.find defhash v)
	  with Not_found ->
	    (* Variable not found.  It is a free variable. *)
	    Ast.Var(v)
	end
    | Ssa.Int(i,t) -> Ast.Int(i,t)
    | _ -> raise Not_simple_mem;
  in ssavaltoastexp ssaval

(* This function, given an expression from backprop, decides if the
   expression is "constant".  The primary motivation is to declare
   addresses on the stack as constant. *)
let rec isconst = function
  | Ast.BinOp(_,e1,e2) -> isconst e1 && isconst e2
  | Ast.Int(_,_) -> true
  | Ast.Var(_) -> true (* Assuming only vars are free vars *)
  | Ast.Cast(_,_,e) -> isconst e
  | _ -> false

(* Wrapper that calls backprop and isconst *)
let isssavalconst defhash ssaval =
  try
    let bp = backprop defhash ssaval in
    isconst bp
  with 
    Not_simple_mem -> false

(* Convert a program *)
let convert_g p mode =

  (* 
     I can't think of a nice way of transforming memory references to
     scalars, since memory operations are side-effect free and scalars
     are not.  So, I just do a pattern match for the common cases, e.g.,

     mem = store(mem, addr, blah) -> mem_addr = blah

     or

     load(mem,addr) -> mem_addr

     In particular, this doesn't handle mem = store(store(...), addr,
     blah) or load(store(...),addr).
  *)


  let vh = Hashtbl.create 500 in
  let getnewvar mem idx t =
    try Hashtbl.find vh (idx)
    with Not_found ->
      let nv = Var.newvar "memscalar" t
      in Hashtbl.add vh (idx) nv;
      nv
  in
  (* Vis1 simply detects whether or not all memory references can be
     scalarized or not. *)
  let vis1 nonconst bph = object(self)
    inherit Ssa_visitor.nop
      
    method visit_stmt = function
      | Move(lv, e, a) ->
	  begin
	    match lv with
	    | Var.V(i, s, Type.TMem(index)) ->
		begin
(* 		  dprintf "A mem move: %s" (Pp.ssa_stmt_to_string stmt); *)
		  match e with
		  | Store(Var(Var.V(i', _, Type.TMem(_))), idx, ssavalue, endian, t) ->
		      if not (isssavalconst bph idx) then
			begin
			  nonconst := true;
			end;
		      `DoChildren
		  | Val(Var(Var.V(_, _, Type.TMem(_)))) ->
		      (* mem1 = mem2.  Since we assume these are all the same, doesn't matter *)
		      `DoChildren
		  | Phi(_) -> `DoChildren
		  | _ ->
		      pwarn "Some strange memory instruction is present.";
		      `DoChildren

		end
	    | _ ->
(* 		dprintf "A non-mem move: %s" (Pp.ssa_stmt_to_string stmt); *)
		`DoChildren
	  end
	    
      | _ -> `DoChildren
      
    method visit_exp = function
      | Load(Var(arr), idx, endian, t) ->
	  if not (isssavalconst bph idx) then 
	    nonconst := true;
	  `DoChildren
      | _ -> `DoChildren
  end
  in
  (* Vis2 converts statements to their scalarized form. *)
  let vis2 bph = object(self)
    inherit Ssa_visitor.nop
      
    method visit_stmt = function
      | Move(lv, e, a) ->
	  begin
	    match lv with
	    | Var.V(i, s, Type.TMem(index)) ->
		begin
(* 		  dprintf "A mem move: %s" (Pp.ssa_stmt_to_string stmt); *)
		  match e with
		  | Store(Var(Var.V(i', _, Type.TMem(_)) as srcvar), idx, ssavalue, endian, t) ->
		      (* Only convert to scalar if constant reference *)
		      if isssavalconst bph idx then
			begin
			  let nv = getnewvar srcvar idx t in
(* 			  dprintf "%s = %s" (Pp.var_to_string nv) (Pp.value_to_string ssavalue); *)
			  `ChangeToAndDoChildren (Move(nv, (Val ssavalue), []))
			end 
		      else
			`DoChildren
		  | Val(Var(Var.V(_, _, Type.TMem(_)))) ->
		      (* mem1 = mem2.  Since we assume these are all the same, doesn't matter *)
		      `DoChildren
		  | Phi(_) -> `DoChildren
		  | _ ->
		      pwarn "Some strange memory instruction is present.";
		      `DoChildren

		end
	    | _ ->
(* 		dprintf "A non-mem move: %s" (Pp.ssa_stmt_to_string stmt); *)
		`DoChildren
	  end
	    
      | _ -> `DoChildren
      
    method visit_exp = function
      | Load(Var(arr), idx, endian, t) ->
	  if isssavalconst bph idx then 
	    begin
	      let nv = getnewvar arr idx t in
(*  	      dprintf "Read %s: %s" (Pp.var_to_string nv) (Pp.ssa_exp_to_string e); *)
	      `ChangeToAndDoChildren (Val (Ssa.Var nv))
	    end 
	  else
	    `DoChildren
      | _ -> `DoChildren
  end
  in
  let bph = VH.create 1000 in
  let nonconst = ref false in
  buildbtbl bph p;
  let vis1 = vis1 nonconst bph in
  let vis2 = vis2 bph in
  ignore(Ssa_visitor.prog_accept vis1 p);
  dprintf "Found non-constant memory references: %b" !nonconst;
  if !nonconst then
    wprintf "Found non-constant memory references.  Make sure you understand what memory2scalar is going to do in %s mode" (readmode_to_string mode);
  G.fold_vertex
    (fun bb g ->
       let oldstmts = C.get_stmts p bb in
       let newstmts = List.fold_left
	 (fun l stmt ->
	    let isstore, isinitro, lv, rv =
	      match stmt with
	      | Move(lv,Store(Var(Var.V(_, _, Type.TMem(_))) as rv,_,_,_,_),attrs) -> 
		  let isinitro = List.mem Type.InitRO attrs
		  in
		  true, isinitro, Some(lv), Some(rv) 
	      | _ -> false, false, None, None
	    in
	    let newstmt = Ssa_visitor.stmt_accept vis2 stmt in
	    (* If there is a store, there are some non-constant memory
	       references, and there is a scalarized version of this
	       statement, then we need to add both. *)
	    let scond = isstore && !nonconst && newstmt <> stmt in
	    match mode with
	    | Default ->
		if scond then 
		  l @ [stmt; newstmt]
		else
		  l @ [newstmt]
	    | IndirectROPTIR ->
		(* If the operation is from initro, then use the old
		   (memory) statement.  Otherwise, use the new (scalar)
		   statement. *)
		if scond && isinitro then
		  l @ [stmt]
		else if scond then
		  let lv = match lv with 
		    | Some(x) -> x 
		    | _ -> failwith "Assert error" in
		  let rv = match rv with 
		    | Some(x) -> x
		    | _ -> failwith "Assert error" in
		  let movemem = Move(lv, Val(rv), []) in
		    l @ [movemem; newstmt]
		else
		  l @ [newstmt]
	 ) [] oldstmts
       in
(*        List.iter (fun stmt -> *)
(* 		    dprintf "test: %s" (Pp.ssa_stmt_to_string stmt)) newstmts; *)
       C.set_stmts g bb newstmts
    ) p p

