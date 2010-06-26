(* A module to perform trace analysis *)

open Symbeval
open Type
open Ast

module D = Debug.Make(struct let name = "TraceEval" and default=`Debug end)
open D


(* A hash from variable names to concrete values *)
let concrete: (string, Ast.exp) Hashtbl.t = Hashtbl.create 5 
(* A hash from memory indices to concrete values *)
let concrete_mem: (int64, Ast.exp) Hashtbl.t = Hashtbl.create 5 
(* A hash from indices to symbolic vars for each memory location *)
let symb_mem: (int64, Ast.exp) Hashtbl.t = Hashtbl.create 5 

(** So here's how we will do partial symbolic execution on
    traces: 
    1. A trace is a list of AST stmts as executed by the
    program
    2. Execute the trace and at each instruction:
    
    a) check if it is a taint introduction stmt
    b) if it is, update the memory context with the symbolic
    variables
    c) If it a regular stmt, read the new concrete values and
    taint flags and store them in a map
    d) whenever the symbolic evaluator requests a value that is
    known and untainted, provide it with the value from the map
      - if it is tainted let the evaluator worry about it

*)
    
module TaintSymbolic = 
struct 
  let lookup_var delta var = 
    try VH.find delta var
    with Not_found -> 
      (* It HAS to be concrete *)
      let name = Var.name var in
	pdebug ("need the concrete value of " ^ name) ;
	(try Symbolic(Hashtbl.find concrete name)
	 with Not_found ->
	   pdebug ("var not found " ^ name);
	   Symbolic.lookup_var delta var
	)
	  
 
(*	 with Not_found ->
	   pdebug ("did not find concrete for "  ^ name) ;
	   Symbolic.lookup_var delta var*)
	  
  let conc2symb = Symbolic.conc2symb
  let normalize = Symbolic.normalize
  let update_mem = Symbolic.update_mem 
  let lookup_mem mu index endian = 
    (*pdebug ("index at " ^ (Pp.ast_exp_to_string index)) ;*)
    match mu, index with
      | ConcreteMem(m,v), Int(n,t) ->
	  pdebug "searching symb_mem" ;
	  (try 
	     let var = Hashtbl.find symb_mem n in
	       pdebug ("introducing symbolic: "^(Pp.ast_exp_to_string var)) ;
	       (*update_mem mu index var endian;
	       Hashtbl.remove n;*)
	       var
	   with Not_found ->
	     pdebug ("not found in symb_mem "^(Printf.sprintf "%Lx" n)) ;
	     (try AddrMap.find (normalize n t) m
	      with Not_found ->
		(* It HAS to be concrete *)
		(try Hashtbl.find concrete_mem n
		 with Not_found -> 
		   pdebug ("memory not found at "
			   ^(Printf.sprintf "%Lx" n));
		   Symbolic.lookup_mem mu index endian
		)
	     )
	  )
      | _, _ ->
	  (*pdebug "symbolic memory??" ;*)
	  Symbolic.lookup_mem mu index endian
end
  
module TaintConcrete = 
struct 
  let lookup_var delta var = 
    let name = Var.name var in
      (*pdebug ("asking for " ^ name) ;*)
      try Symbolic(Hashtbl.find concrete name) 
      with Not_found ->
	(*pdebug ("did not find concrete for "  ^ name) ;*)
	Symbolic.lookup_var delta var
	  
  let conc2symb = Symbolic.conc2symb
  let normalize = Symbolic.normalize
  let update_mem = Symbolic.update_mem 
  let lookup_mem mu index endian = 
    (*pdebug ("index at " ^ (Pp.ast_exp_to_string index)) ;*)
    match index with
      | Int(n,t) ->
	  (try Hashtbl.find concrete_mem n
	   with Not_found ->
	     (*pdebug ("memory not found at "
		     ^ (Printf.sprintf "%Lx" n));*)
	     Symbolic.lookup_mem mu index endian
	  )
      | _ ->
	  (*pdebug "symbolic memory??" ;*)
	  Symbolic.lookup_mem mu index endian
end
  
module TraceSymbolic = Symbeval.Make(TaintSymbolic)(FullSubst)
module TraceConcrete = Symbeval.Make(TaintConcrete)(FullSubst)

(* Useful shorthands to manipulate Taint attributes *)
let keep_taint = function 
  | Context _ -> true 
  | _ -> false 
      
let unwrap_taint = function 
  | Context c -> c 
  | _ -> failwith "trying to unwrap a non-taint attribute"
      
(* Keeping only the attributes that contain taint info *)
let filter_taint atts = 
  let atts = List.filter keep_taint atts in
    List.map unwrap_taint atts     

(* The number of bytes needed to represent each type *) 
let typ_to_bytes = function 
  | Reg 1 | Reg 8 -> 1
  | Reg 16 -> 2
  | Reg 32 -> 4
  | Reg 64 -> 8
  | _ -> failwith "not a register" 

(* Get the ith byte of a value v *)
let get_byte i v = 
  Int64.logand (Int64.shift_right v ((i-1)*8)) 0xffL 

let num_to_bit num =
  if num > Int64.zero then Int64.one else Int64.zero

let add_eflags eflags =
  Hashtbl.add concrete "R_ZF" (Int(num_to_bit (Int64.logand eflags 0x40L), reg_1))

(* Add a mapping if there doesn't already exist *)
let add_if_not_exists hash key value typ =
  if not (Hashtbl.mem hash key) then
    (if key = "EFLAGS" then
       add_eflags value ;
     Hashtbl.add hash key (Int(value,typ))
    )

(* Stores the concrete (known) memory bytes in concrete_mem *)
let rec add_mem index value limit = function
  | 0 -> ()
  | n ->
      let byte = get_byte (limit-n+1) value in
        if not (Hashtbl.mem concrete_mem index) then
	  Hashtbl.add concrete_mem index (Int(byte,reg_8)) ;
        add_mem (Int64.succ index) value limit (n-1)

(* Store the concrete information in tables *)
let add_to_conc accept 
    {name=name; mem=mem; index=index; 
     value=value; t=typ; taint=taint} =
  (*if mem then pdebug ("concrete memory at "
		      ^(Printf.sprintf "%Lx:" index)
		     ^(string_of_int (typ_to_bytes typ))) ;
  *)
  match taint with
    | Taint n when mem && accept n -> 
        let limit = typ_to_bytes typ in
	  add_mem index value limit limit 
    | Taint n when accept n ->
	(* assert (Hashtbl.mem concrete name = false) ; *)
	add_if_not_exists concrete name value typ
    | _ -> () (* tainted, do nothing *)	     

let counter = ref 1

(* Initializing the lookup tables *)
let init_taint_tables () =
  (*pdebug ("label no: " ^ (string_of_int !counter)) ;*)
  Hashtbl.clear concrete;
  Hashtbl.clear concrete_mem

(* Updating the lookup tables with the concrete values *)
let update_concrete accept = function
  | Label (_,atts) -> 
      let conc_atts = filter_taint atts in
        if conc_atts != [] then init_taint_tables ();
        List.iter (add_to_conc accept) conc_atts
  | _ -> ()

(** An evaluator for each stmt of the trace *)
let eval state = 
  try
    let stmt = TraceSymbolic.inst_fetch state.sigma state.pc in
      (*pdebug (Pp.ast_stmt_to_string stmt) ; *)
      (*update_concrete stmt ; FIXMEFIXMEFIXME*)
      TraceSymbolic.eval_stmt state stmt
  with
    | Failure fail -> 
	(try
	   let prev = TraceSymbolic.inst_fetch state.sigma (Int64.pred state.pc) in
	     failwith (fail^"\nprev: "^(Pp.ast_stmt_to_string prev))
	 with Failure _ -> 
	   failwith (fail^" no prev")
	)
    | Halted _ -> 
	pdebug "Halted!!! ... generating predicate";
	let oc = open_out "predicate.txt" in
	let wp = state.pred in
	(* pdebug (Pp.ast_exp_to_string state.pred) ; *)
	let m2a = new Memory2array.memory2array_visitor () in
	let wp = Ast_visitor.exp_accept m2a wp in
	let foralls = List.map (Ast_visitor.rvar_accept m2a) [] in
	let p = new Stp.pp_oc oc in
	let () = p#assert_ast_exp_with_foralls foralls wp in
	  p#close;
	  failwith "success!!!!"
	    
(*  Set of Assumptions:
 *
 *  - All instruction-internal jmps use labels  *
 *    of the form: L_<integer>                  *)
let is_internal_label s = s.[0] = 'L'

(*  - All address labels start with "pc_0x"     *)
let is_addr_label l = 
  String.length l > 5 && String.sub l 0 5 = "pc_0x"

(** Get the address of the next instruction in the trace *)
let rec get_next_address = function
  | [] -> raise Not_found
  | (Ast.Label ((Addr n),_))::_ -> 
      Name ("pc_"^(Int64.format "0x%Lx" n))
  | _::xs -> get_next_address xs     

(* Converts an address to a string label *)
let to_label = function
  | Addr n -> Name ("pc_"^(Int64.format "0x%Lx" n))
  | other -> other

(** Fetching the first stmt with attributes containing taint info *)
let rec get_first_atts = function
  | [] -> failwith "no taint analysis info were found in the trace"
  | (Ast.Label (_,atts))::rest ->
      let taint_atts = filter_taint atts in
	if taint_atts <> [] then (taint_atts, rest)
	else get_first_atts rest
  | s::rest -> 
      get_first_atts rest 

(** Symbolically executing the trace and at the same
    time concretizing as much as possible *)
let execute_trace st =
  let rec execute_next = function
    | []   -> pdebug "execution complete"
    | [st] -> execute_next (eval st)
    | _ -> failwith "more than one execution paths found on trace"
  in
    execute_next [st]
      
(** Initializing the trace contexts *)
let init_trace trace ctx = 
  let atts,_ = get_first_atts trace in
    (* Create a memory to place the initial symbols *)
  List.iter
    (fun {index=index; taint=Taint taint} ->
       let varname = "symb_"^(string_of_int taint) in
       let newvar = Var (Var.newvar varname reg_8) in
	 Hashtbl.add symb_mem index newvar
    ) atts;
    pdebug "Added the initial symbolic seeds" 
      
let jump_targets = ref []

(** Converting cjmps to asserts whenever possible
    Based on the observation that all instructions
    contained in the trace are sequential *)
let cjmps_to_asserts = 
  let rec cjmps_to_asserts acc = function
	| [] -> List.rev acc
	| (Ast.CJmp (e,_,_,atts1))::(Ast.Label (_,_) as l)::xs ->
	    let taken = List.hd !jump_targets in
	    let newstmt = 
	      if taken then [l ; Ast.Assert(e,atts1)]
	      else [l ; Ast.Assert(UnOp(NOT,e),atts1)]
		(*match lab_of_exp l1, lab_of_exp l2, to_label l3 with
		  | Some lab1, _, lab3 when lab1 = lab3 ->*)
		(*	  | _, Some lab2, lab3 when lab2 = lab3 ->*)
		(*	  | _ ->
			  pdebug ("Current label "
			  ^(Pp.ast_exp_to_string l1)
			  ^ " =?= "
			  ^(Pp.ast_exp_to_string (exp_of_lab l3)));
			  failwith "could not resolve cjmp target"*)
	    in
	      jump_targets := List.tl (List.tl !jump_targets) ;
	      cjmps_to_asserts (newstmt@acc) xs
	| x::xs ->
	    jump_targets := List.tl !jump_targets ;
	    cjmps_to_asserts (x::acc) xs
  in
    cjmps_to_asserts []
 
(** Removing all jumps from the trace *)
let remove_jumps =
  let no_jmps = function 
    | Ast.Jmp _ -> false 
    | _ -> true
  in
    List.filter no_jmps

(** Removing all specials from the traces *)	
let remove_specials =
  let no_specials = function 
    | Ast.Special _ -> false 
    | _ -> true
  in
    List.filter no_specials

(* Appends a Halt instruction to the end of the trace *)
let append_halt trace = 
  let halt = Ast.Halt (exp_true, []) in
    trace@[halt]
      
(** A trace is a sequence of instructions. This function
    takes a list of ast statements and returns a list of
    lists of ast stmts. Each one of those sublists will 
    represent the IL of the executed assembly instruction *)
let trace_to_blocks trace = 
  let rec to_blocks blocks current = function
    | [] -> 
	List.rev ((List.rev current)::blocks)
    | (Ast.Label (Addr _, _) as l)::rest ->
	let block = List.rev current in
	to_blocks (block::blocks) [l] rest
    | x::rest ->
	to_blocks blocks (x::current) rest
  in
  let blocks = to_blocks [] [] trace in
    List.filter (fun b -> List.length b > 1) blocks

(** Strips the last jump of the block *)
let strip_jmp block =
  match List.rev block with
	 | (Ast.Jmp _)::rest -> List.rev rest
	 | _ -> block
	     
let print_block =
  List.iter (fun s -> pdebug (Pp.ast_stmt_to_string s))

let hd_tl = function
  | [] -> failwith "empty list"
  | x::xs -> x, xs

(** Running each block separately *)
let run_block state block = 
  (*pdebug ("Running block: " ^ (string_of_int !counter));*)
  counter := !counter + 1 ;
  let addr, block = hd_tl block in
  let info, block = hd_tl block in
  let accept_taint = fun _ -> true in
  let _ = update_concrete accept_taint info in
  let block = append_halt block in 
  let block = strip_jmp block in
  (*print_block block ;*)
  TraceConcrete.initialize_prog state block ;
  let init = TraceConcrete.inst_fetch state.sigma state.pc in
  let executed = ref [] in
  let rec eval_block state stmt = 
    (*pdebug (Pp.ast_stmt_to_string stmt);*)
    (*    Hashtbl.iter (fun k v -> pdebug (Printf.sprintf "%Lx -> %s" k (Pp.ast_exp_to_string v))) concrete_mem ;*)
    let result = match stmt with
      | Ast.CJmp (cond, _, _, _) ->
	  TraceConcrete.eval_expr state.delta cond = val_true
      | _ -> false
    in
    executed := (stmt,result) :: !executed ; 
    match TraceConcrete.eval_stmt state stmt with
      | [newstate] ->
	  let next = TraceConcrete.inst_fetch newstate.sigma newstate.pc in
	    (*pdebug ("pc: " ^ (Int64.to_string newstate.pc)) ;*)
	    eval_block newstate next
      | _ -> 
	  failwith "multiple targets..."
  in
    try
      eval_block state init
    with 
      |	Failure s -> 
	  pdebug ("block evaluation failed :(\nReason: "^s) ;
	  List.iter (fun s -> pdebug (Pp.ast_stmt_to_string s)) block ;
	  ((addr,false)::(info,false)::(List.tl !executed))
      | UnknownLabel ->
	  ((addr,false)::(info,false)::List.rev !executed)
      | Halted _ -> 
	  ((addr,false)::(info,false)::List.rev (List.tl !executed))

let run_blocks blocks =
  let state = TraceConcrete.create_state () in
  let rev_trace = List.fold_left 
    (fun acc block -> 
       (run_block state block)::acc
    ) [] blocks
  in
    List.flatten (List.rev rev_trace)
      
let add_symbolic_seeds header state =
  (*let var = Var.newvar "mem" (TMem reg_8) in
  let init_mem = empty_mem var in
  *)
  let atts,_ = get_first_atts [header] in
    (*pdebug ("Attr length: " ^ (string_of_int (List.length atts)));*)
  (*let newmem = *)
  List.iter
    (fun {index=index; taint=Taint taint} ->
       let newvarname = "symb_" ^ (string_of_int taint) in
       let sym_var = Var (Var.newvar newvarname reg_8) in
	 (*pdebug ((Printf.sprintf "%Lx" index)^" -> "^(Pp.ast_exp_to_string sym_var));*)
	 Hashtbl.add symb_mem index sym_var
       (*let index = Int(index,reg_32) in
       let endian = exp_false in
	 TraceSymbolic.update_mem mem index sym_var endian*)
    ) atts
  (*in TraceSymbolic.context_update state.delta var newmem*)
    
  
let output_formula state = 
  let oc = open_out "predicate.txt" in
  let wp = state.pred in
    (* pdebug (Pp.ast_exp_to_string state.pred) ; *)
  let m2a = new Memory2array.memory2array_visitor () in
  let wp = Ast_visitor.exp_accept m2a wp in
  let foralls = List.map (Ast_visitor.rvar_accept m2a) [] in
  let p = new Stp.pp_oc oc in
  let () = p#assert_ast_exp_with_foralls foralls wp in
  let () = p#counterexample () in
    p#close;
    pdebug "success!!!!";
    state
      
let symbolic_run trace = 
  counter := 0 ;
  let trace = List.tl trace in
  let header, trace = hd_tl trace in
  let trace = append_halt trace in
  let state = TraceSymbolic.build_default_context trace in
  add_symbolic_seeds header state ;
    List.fold_left 
      (fun state stmt ->
	 update_concrete (fun _ -> true) stmt ;
	 (match stmt with
	   | Ast.Label (_,atts) when filter_taint atts != [] -> 
	       (*pdebug ("stmt no: " ^ (string_of_int !counter));*)
	       counter := !counter + 1 ;
	   | _ -> ());
	 try
	   (match TraceSymbolic.eval_stmt state stmt with
	     | [next] -> next
	     | _ -> failwith "Jump in a straightline program"
	   )
	 with 
	   | Failure fail -> 
	       pdebug (fail^"\nprev: "^(Pp.ast_stmt_to_string stmt));
	       output_formula state 
	   | Halted _ -> 
	       pdebug "Halted!!! ... generating predicate";
	       output_formula state
	   | _ -> 
	       (*TraceSymbolic.print_values state.delta;*)
	       pdebug ("Reason: "^(Pp.ast_stmt_to_string stmt));
	       output_formula state
		 
      ) state trace     

(** Perform concolic execution on the trace and
    output a set of constraints *)
let concrete trace = 
  let no_specials = remove_specials trace in
  let blocks = trace_to_blocks no_specials in
  (*pdebug ("blocks: " ^ (string_of_int (List.length blocks)));*)
  let actual_trace = run_blocks blocks in
  jump_targets := List.map snd actual_trace ;
  let actual_trace = List.map fst actual_trace in
  let straightline = cjmps_to_asserts actual_trace in
  let no_jumps = remove_jumps straightline in
    no_jumps

(* Substituting the last jump with assertions *)
let convert trace = 
  let rec sub = function
    | [] -> failwith "no jump found"
    | (Ast.Jmp(e, atts))::rest ->
	let e' = BinOp(EQ,e,Int(0x08048554L,reg_32)) in
	(Ast.Assert(e', atts))::rest
    | _::rest -> sub rest
  in
  let rev = List.rev trace in
  let clean = sub rev in
  let trace = List.rev clean in
    (* FIXME: more processing *)
    trace
	

let concolic trace = 
  let trace = convert trace in
  let trace = concrete trace in
  ignore (symbolic_run trace) ;
  trace


(*;
  let trace = append_halt no_specials in*)
(*  let ctx = Symbolic.build_default_context trace in
    init_trace trace ctx ;
    execute_trace ctx ; *)
    (*trace*)

