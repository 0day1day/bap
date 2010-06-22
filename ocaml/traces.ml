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

let find delta var =
  pdebug "my find";
  let name1 = Var.name var in
  let all = VH.fold 
    (fun k v acc ->
       let name2 = Var.name k in
	 if name1 = name2 then (v::acc)
	 else acc
    ) delta []
  in
    try List.hd all
    with _ -> 
      pdebug (name1 ^ " not found");
      raise Not_found    
    
module TaintLookup = 
struct 
  let lookup_var delta var = 
    try find delta var 
    with Not_found ->
      (let name = Var.name var in
	 pdebug ("asking for " ^ name) ;
	 try Symbolic(Hashtbl.find concrete name) 
	 with Not_found ->
	   pdebug "found concrete" ;
	   Symbolic.lookup_var delta var
      )
  let conc2symb = Symbolic.conc2symb
  let normalize = Symbolic.normalize
  let update_mem = Symbolic.update_mem 
  let lookup_mem mu index endian = 
    pdebug ("index at " ^ (Pp.ast_exp_to_string index)) ;
    match mu, index with
      | ConcreteMem(m,v), Int(n,t) ->
	  pdebug "concrete mem";
	  (try AddrMap.find (normalize n t) m
	   with Not_found ->
	     pdebug "did not find it, looking in trace";
	     (try Hashtbl.find concrete_mem n
	      with Not_found ->
		pdebug "not found :(";
		Symbolic.lookup_mem mu index endian)
	  )
      | _,_ ->
	  pdebug "symbolic memory??" ;
	  Symbolic.lookup_mem mu index endian
end
  
module Trace = Symbeval.Make(TaintLookup)(FullSubst)

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

(* Add a mapping if there doesn't already exist *)
let add_if_not_exists hash key value =
  if not (Hashtbl.mem hash key) then Hashtbl.add hash key value

(* Stores the concrete (known) memory bytes in concrete_mem *)
let rec add_mem index value limit = function
  | 0 -> ()
  | n ->
      let byte = get_byte (limit-n+1) value in
        add_if_not_exists concrete_mem index (Int(byte,reg_8)) ;
        add_mem (Int64.succ index) value limit (n-1)

(* Store the concrete information in tables *)
let add_to_conc {name=name; mem=mem; index=index; 
		 value=value; t=typ; taint=taint} =
  match taint with
    | Taint 0 when mem -> 
        let limit = typ_to_bytes typ in
	  add_mem index value limit limit 
    | Taint 0 ->
	(* assert (Hashtbl.mem concrete name = false) ; *)
	add_if_not_exists concrete name (Int(value, typ))
    | _ -> () (* tainted, do nothing *)	     

(* Initializing the lookup tables *)
let init_taint_tables () =
  Hashtbl.clear concrete;
  Hashtbl.clear concrete_mem

(* Updating the lookup tables with the concrete values *)
let update_concrete = function
  | Label (_,atts) -> 
      let conc_atts = filter_taint atts in
        if conc_atts != [] then init_taint_tables ();
        List.iter add_to_conc conc_atts
  | _ -> ()

(** An evaluator for each stmt of the trace *)
let eval state = 
  try
    let stmt = Trace.inst_fetch state.sigma state.pc in
      (*pdebug (Pp.ast_stmt_to_string stmt) ; *)
      update_concrete stmt ;
      Trace.eval_stmt state stmt
  with
    | Failure fail -> 
	(try
	   let prev = Trace.inst_fetch state.sigma (Int64.pred state.pc) in
	     failwith (fail^"\nprev: "^(Pp.ast_stmt_to_string prev))
	 with Failure _ -> 
	   failwith (fail^" no prev")
	)
    | Halted _ -> 
	let oc = open_out "predicate.txt" in
	let wp = state.pred in
	  pdebug (Pp.ast_exp_to_string state.pred) ;
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
  let memory = 
    ConcreteMem(
      AddrMap.empty,
	Var.newvar ("mem") (TMem reg_32)) 
  in 
  let _memory = List.fold_left (* FIXME: registers *)
    (fun (memory:varval) {index=index; taint=Taint taint} ->
       let varname = "symb"^(string_of_int taint) in
       let newvar = Var (Var.newvar varname reg_8)
       and index = (Int(index,reg_32)) in
	 Symbolic.update_mem memory newvar index exp_false
    ) memory atts
  in
    pdebug "starting ..." 
	
(** Converting cjmps to asserts whenever possible
    Based on the observation that all instructions
    contained in the trace are sequential *)
let cjmps_to_asserts = 
  let rec cjmps_to_asserts acc = function
    | [] -> List.rev acc
    | (Ast.CJmp (e,l1,l2,atts) as x)::xs ->
	let newstmt = match lab_of_exp l1 with
	  | Some (Name s as a) when not (is_internal_label s) ->
	      let true_branch = [Ast.Jmp(l1,[]) ; Ast.Assert(e,atts)] in
	      let false_branch = [Ast.Jmp(l2,[]) ; Ast.Assert(UnOp(NOT,e),atts)] in
		(try 
		   if get_next_address xs = a then true_branch
		   else false_branch
		 with Not_found -> false_branch
		)
	  | _ -> [x]
	in
	  cjmps_to_asserts (newstmt@acc) xs
    | x::xs -> cjmps_to_asserts (x::acc) xs
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
      
(** Perform concolic execution on the trace and
    output a set of constraints *)
let concolic trace = 
  let no_specials = remove_specials trace in
  let no_cjmps = cjmps_to_asserts no_specials in
  let no_jmps = remove_jumps no_cjmps in
  let trace = append_halt no_jmps in
  let ctx = Symbolic.build_default_context trace in
    init_trace trace ctx ;
    execute_trace ctx ;
    trace

