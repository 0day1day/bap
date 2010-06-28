(* A module to perform trace analysis *)

open Symbeval
open Type
open Ast

module D = Debug.Make(struct let name = "TraceEval" and default=`Debug end)
open D

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

(*************************************************************)
(**********************  Datastructures  *********************)
(*************************************************************)

(* The datastructures that are be used during trace analysis *)

(* A type for all concrete values *)
type value = 
{
  exp: Ast.exp;
  tnt: bool;
}

type environment =
{
  vars:     (string,value)  Hashtbl.t;
  memory:   (int64,value)   Hashtbl.t;
  symbolic: (int64,Ast.exp) Hashtbl.t;
}

(* A global environment to keep the concrete and taint 
   information of the statement block that is analyzed *)
let global = 
{
  vars     = Hashtbl.create 10;
  memory   = Hashtbl.create 10;
  symbolic = Hashtbl.create 10;
}

(* Some wrappers to interface with the above datastructures *)

let var_lookup = Hashtbl.find global.vars
let mem_lookup = Hashtbl.find global.memory

let concrete_val name = (var_lookup name).exp
let concrete_mem index = (mem_lookup index).exp
let symbolic_mem = Hashtbl.find global.symbolic

let bound = Hashtbl.mem global.vars
let in_memory = Hashtbl.mem global.memory

let add_var var value taint = 
  Hashtbl.add global.vars var {exp=value;tnt=taint;}
let add_mem index value taint =
  Hashtbl.add global.memory index {exp=value;tnt=taint;}
let add_symbolic = Hashtbl.add global.symbolic
  
let add_new_var var value taint = 
  if not (bound var) then 
    add_var var value taint

let cleanup () =
  Hashtbl.clear global.vars;
  Hashtbl.clear global.memory


(*************************************************************)
(*********************  Helper Functions  ********************)
(*************************************************************)

  
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

(* Wrappers & Useful shorthands to manipulate taint 
   attributes and process taint info *)
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

let taint_to_bool n = n != 0

let hd_tl = function
  | [] -> failwith "empty list"
  | x::xs -> x, xs

(* Unfortunately we need to special-case the EFLAGS registers
   since PIN does not provide us with separate registers for 
   the zero, carry etc flags *)
let add_eflags eflags taint =
  add_var 
    "R_ZF" 
    (Int(num_to_bit (Int64.logand eflags 0x40L), reg_1))
    taint;
  add_var 
    "R_CF" 
    (Int(num_to_bit (Int64.logand eflags 0x01L), reg_1))
    taint
 (* TODO: handle more EFLAGS registers *)
	  
(* Store the concrete taint info in the global environment *)
let add_to_conc  {name=name; mem=mem; index=index; 
		  value=value; t=typ; taint=Taint taint} =
  (* Stores the concrete (known) memory bytes in the global 
     environment in little endian order *)
  let add_to_mem index value taint limit = 
    let rec add_mem_aux index = function
      | 0 -> 
	  ()
      | n -> 
	  let byte = get_byte (limit-n+1) value in
            if not (in_memory index) then
	      add_mem index (Int(byte,reg_8)) taint ;
            add_mem_aux (Int64.succ index) (n-1)
    in
      add_mem_aux index
  in
  let taint = taint_to_bool taint in 
    if mem then
      let limit = typ_to_bytes typ in
	add_to_mem index value taint limit limit 
    else
      (* assert (Hashtbl.mem concrete name = false) ; *)
      add_new_var name (Int(value,typ)) taint ;
    if name = "EFLAGS" then add_eflags value taint
      
(* Updating the lookup tables with the concrete values *)
let update_concrete = function
  | Label (_,atts) -> 
      let conc_atts = filter_taint atts in
        if conc_atts != [] then cleanup ();
        List.iter add_to_conc conc_atts
  | _ -> ()

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
      
(** Initializing the trace contexts *)
let init_trace trace ctx = 
  let atts,_ = get_first_atts trace in
    (* Create a memory to place the initial symbols *)
  List.iter
    (fun {index=index; taint=Taint taint} ->
       let varname = "symb_"^(string_of_int taint) in
       let newvar = Var (Var.newvar varname reg_8) in
	 add_symbolic index newvar
    ) atts;
    pdebug "Added the initial symbolic seeds" 
 
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

(*************************************************************)
(********************  Concrete Execution  *******************)
(*************************************************************)

module TaintConcrete = 
struct 
  let lookup_var delta var = 
    (* Check if we know the concrete value *)
    try Symbolic(concrete_val (Var.name var)) 
    with Not_found ->
      (* otherwise do a symbolic lookup *)
      (*pdebug ("did not find concrete for "  ^ name) ;*)
      Symbolic.lookup_var delta var
	  
  let conc2symb = Symbolic.conc2symb
  let normalize = Symbolic.normalize
  let update_mem = Symbolic.update_mem 
  let lookup_mem mu index endian = 
    (*pdebug ("index at " ^ (Pp.ast_exp_to_string index)) ;*)
    match index with
      | Int(n,t) ->
	  (try concrete_mem n
	   with Not_found ->
	     (*pdebug ("memory not found at "
	       ^ (Printf.sprintf "%Lx" n));*)
	     Symbolic.lookup_mem mu index endian
	  )
      | _ ->
	  (*pdebug "symbolic memory??" ;*)
	  Symbolic.lookup_mem mu index endian
end
  
module TraceConcrete = Symbeval.Make(TaintConcrete)(FullSubst)

let counter = ref 1
      
(** Running each block separately *)
let run_block state block = 
  pdebug ("Running block: " ^ (string_of_int !counter));
  counter := !counter + 1 ;
  let addr, block = hd_tl block in
  let info, block = hd_tl block in
  let _ = update_concrete info in
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
  counter := 1 ;
  let state = TraceConcrete.create_state () in
  let rev_trace = List.fold_left 
    (fun acc block -> 
       (run_block state block)::acc
    ) [] blocks
  in
    List.flatten (List.rev rev_trace)
 
(** Converting cjmps to asserts. We use the results of
    the concrete execution of the trace in order to 
    determine the jump targets. *)
let cjmps_to_asserts = 
  let rec cjmps_to_asserts acc = function
    | [] -> List.rev acc
    | (Ast.CJmp (e,_,_,atts1),true)::(Ast.Label (_,_) as l,_)::xs ->
	cjmps_to_asserts ([l ; Ast.Assert(e,atts1)]@acc) xs
    | (Ast.CJmp (e,_,_,atts1),false)::(Ast.Label (_,_) as l,_)::xs ->
	cjmps_to_asserts ([l ; Ast.Assert(UnOp(NOT,e),atts1)]@acc) xs
    | (x,_)::xs ->
	cjmps_to_asserts (x::acc) xs
  in
    cjmps_to_asserts []

(** Perform concolic execution on the trace and
    output a set of constraints *)
let concrete trace = 
  let no_specials = remove_specials trace in
  let blocks = trace_to_blocks no_specials in
  (*pdebug ("blocks: " ^ (string_of_int (List.length blocks)));*)
  let actual_trace = run_blocks blocks in
  let straightline = cjmps_to_asserts actual_trace in
  let no_jumps = remove_jumps straightline in
    no_jumps

(*************************************************************)
(********************  Concolic Execution  *******************)
(*************************************************************)

module TaintSymbolic = 
struct 
  let lookup_var delta var = 
    (* Check if we know anything about var *)
    try VH.find delta var
    with Not_found -> 
      (* else it HAS to be concrete *)
      let name = Var.name var in
	pdebug ("need the concrete value of " ^ name) ;
	(try Symbolic(concrete_val name)
	 with Not_found ->
	   (* worst case: create a symbolic variable *)
	   pdebug ("var not found " ^ name);
	   Symbolic.lookup_var delta var
	)
	  
  let conc2symb = Symbolic.conc2symb
  let normalize = Symbolic.normalize
  let update_mem = Symbolic.update_mem 
    
  (* TODO: add a memory initializer *)

  let lookup_mem mu index endian = 
    match mu, index with
      | ConcreteMem(m,v), Int(n,t) ->
	  (try 
	     (* Check if this is a symbolic seed *)
	     let var = symbolic_mem n in
	       pdebug ("introducing symbolic: "^(Pp.ast_exp_to_string var)) ;
	       (*update_mem mu index var endian;
		 Hashtbl.remove n;*)
	       var
	   with Not_found ->
	     (* Check if we know something about this memory location *)
	     (*pdebug ("not found in symb_mem "^(Printf.sprintf "%Lx" n)) ;*)
	     (try AddrMap.find (normalize n t) m
	      with Not_found ->
		(* otherwise it HAS to be concrete *)
		(try concrete_mem n
		 with Not_found -> 
		   pdebug ("memory not found at "
			   ^(Printf.sprintf "%Lx" n));
		   Symbolic.lookup_mem mu index endian
		)
	     )
	  )
      | _, _ ->
	  pdebug ("Symbolic mem: index at " 
		  ^ (Pp.ast_exp_to_string index)) ;
	  Symbolic.lookup_mem mu index endian
end

module TraceSymbolic = Symbeval.Make(TaintSymbolic)(FullSubst)

      
let add_symbolic_seeds header state =
  let atts,_ = get_first_atts [header] in
  List.iter
    (fun {index=index; taint=Taint taint} ->
       let newvarname = "symb_" ^ (string_of_int taint) in
       let sym_var = Var (Var.newvar newvarname reg_8) in
	 pdebug ("Introducing symbolic: " 
		 ^(Printf.sprintf "%Lx" index)
		 ^" -> "
		 ^(Pp.ast_exp_to_string sym_var));
	 add_symbolic index sym_var
    ) atts

let symbolic_run trace = 
  counter := 1 ;
  let trace = List.tl trace in
  let header, trace = hd_tl trace in
  let trace = append_halt trace in
  let state = TraceSymbolic.build_default_context trace in
  add_symbolic_seeds header state ;
    try 
      let state = List.fold_left 
	(fun state stmt ->
	   update_concrete stmt ;
	   (*pdebug (Pp.ast_stmt_to_string stmt);
	   (match stmt with
	      | Ast.Label (_,atts) when filter_taint atts != [] -> 
		  TraceSymbolic.print_var state.delta "R_ESP" 
		  (*pdebug ("block no: " ^ (string_of_int !counter));
		  counter := !counter + 1 ;*)
	      | _ -> ());*)
	   match TraceSymbolic.eval_stmt state stmt with
	     | [next] -> next
	     | _ -> failwith "Jump in a straightline program"
	) state trace
      in
	state.pred
    with 
      | Failure fail -> 
	  pdebug ("Symbolic Run Fail: "^fail);
	  state.pred
      | Halted (_,state) -> 
	  pdebug "Symbolic Run ... Successful!";
	  state.pred
      | _ -> 
	  pdebug "Symbolic Run: Early Abort";
	  (*TraceSymbolic.print_values state.delta;*)
	  (*pdebug ("Reason: "^(Pp.ast_stmt_to_string stmt));*)
	  state.pred
	    
(* Substituting the last jump with assertions *)
let convert addr trace = 
  let target = Int64.of_string ("0x"^addr) in
  let rec sub = function
    | [] -> failwith "no jump found"
    | (Ast.Jmp(e, atts))::rest ->
	let e' = BinOp(EQ,e,Int(target,reg_32)) in
	(Ast.Assert(e', atts))::rest
    | _::rest -> sub rest
  in
  let rev = List.rev trace in
  let clean = sub rev in
  let trace = List.rev clean in
    (* FIXME: more processing *)
    trace
	

let concolic trace = 
  let trace = concrete trace in
  ignore (symbolic_run trace) ;
  trace


(*************************************************************)
(********************  Formula Generation  *******************)
(*************************************************************)

let generate_formula trace = 
  let trace = concrete trace in
    symbolic_run trace
 
let output_formula file trace = 
  let formula = generate_formula trace in
  let oc = open_out file in
    (* pdebug (Pp.ast_exp_to_string state.pred) ; *)
  let m2a = new Memory2array.memory2array_visitor () in
  let formula = Ast_visitor.exp_accept m2a formula in
  let foralls = List.map (Ast_visitor.rvar_accept m2a) [] in
  let p = new Stp.pp_oc oc in
  let () = p#assert_ast_exp_with_foralls foralls formula in
  let () = p#counterexample () in
    p#close;
    pdebug "STP formula generated.";
    trace
      
