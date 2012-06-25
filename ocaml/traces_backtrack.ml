(* Backwards taint analysis on traces *)

(* open Ast *)
(* open Big_int_Z *)
(* open Big_int_convenience *)
(* open Type *)
(* open Exploitable_rules *)
(* open Exploitable_symbeval *)
(* open Solver *)

(* module D = Debug.Make(struct let name = "Exploitable" and default=`Debug end) *)
(* open D *)

(* exception FinalRule *)
(* exception FoundValue of Big_int_Z.big_int *)

(* (\* Global Variables *\) *)
(* let finished = ref false *)
(* let post_mortem = ref false *)
(* let module_names = ref [] *)
(* let module_addrs = ref [] *)
(* let modules_dir = ref "modules" *)

(* (\* XXX: Add 64 bit support *\) *)
(* let isAddrUserMode exp = (exp.offInstr < 0x80000000L) *)
(* let isAddrNearNull exp = (exp.offInstr < 65536L)       (\* 64*1024 *\) *)


(* (\* Utility Functions *\) *)
(* let split d = Str.split (Str.regexp_string d) *)
(* let mod_split s = *)
(*    try *)
(*       let tokens = split "|" s in *)
(*       (List.nth tokens 0, List.nth tokens 1) *)
(*    with Failure _ -> *)
(*       failwith ("Unknown format of module names: \'"^s^"\'") *)

(* let read_module_names filename = *)
(*    let chan = open_in filename in *)
(*    try *)
(*       while true; do *)
(*          let (module_name,module_load_addr) = mod_split (input_line chan) in *)
(*          module_names := module_name :: !module_names; *)
(*          module_addrs := (Util.big_int_of_string module_load_addr) :: !module_addrs *)
(*       done; *)
(*    with End_of_file -> *)
(*       close_in chan; *)
(*       module_names := (List.rev !module_names); *)
(*       module_addrs := (List.rev !module_addrs) *)

(* let load_modules ()= *)
(*    let modules = *)
(*       ( *)
(*          List.map *)
(*          (fun a -> (`Bin a)) *)
(*          ( *)
(*             List.map *)
(*             (fun a -> (!modules_dir^"/"^a)) *)
(*             !module_names *)
(*          ) *)
(*       ) *)
(*    in *)
(*    (fst (Input.get_program_aux ~aslr:!module_addrs (List.rev modules))) *)


(* (\* Reporting Function *\) *)
(* let report_result (r, e) = *)
(*    dprintf ""; *)
(*    dprintf "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="; *)
(*    dprintf "=-         Exploitability Checker Report         -="; *)
(*    dprintf "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="; *)
(*    dprintf " [!] Faulting instruction @ %#Lx" e.from_addr; *)
(*    dprintf " [!] Exception: %s" r.result_short_desc; *)
(*    (match r.result_class with *)
(*    | NOT_AN_EXCEPTION -> dprintf " [!] Impossible!!!" *)
(*    | PROBABLY_NOT_EXPLOITABLE -> dprintf " [!] Exploitability: PROBABLY NOT EXPLOITABLE" *)
(*    | PROBABLY_EXPLOITABLE -> dprintf " [!] Exploitability: PROBABLY EXPLOITABLE" *)
(*    | EXPLOITABLE -> dprintf " [!] Exploitability: EXPLOITABLE" *)
(*    | UNKNOWN -> dprintf " [!] Exploitability: UNKNOWN"); *)
(*    dprintf " [!] Description: %s" r.result_desc; *)
(*    dprintf "                  (%s)" r.result_expl *)

(* (\* XXX: Add handling for non-exception trace *\) *)
(* let get_expt_info = function *)
(*    | Special (_,attrs) -> *)
(*        let exp = *)
(*           (try List.find (fun a -> match a with | Exception e -> true | _ -> false) attrs *)
(*            with Not_found -> failwith "Only supports Exception -- no Exception attr found") *)
(*        in *)
(*          (match exp with *)
(*           | Exception ex -> ex *)
(*           | _ -> failwith "Impossible") *)
(*    | _ -> failwith "Only supports Exception -- needs to be in Special statement" *)


(* Module MV:
 *    - This is a slightly modified version of variable, to incooperate both
 *    variables (Var.t) and the memory location (int64).
 *
 *   - Compare between variable and memory location is arbitrary.
 *)
module MV = struct
   type t = V of Var.t | M of Big_int_Z.big_int
   let compare mv1 mv2 =
      match (mv1,mv2) with
      | ((V x),(V y)) -> compare x y
      | ((M x),(M y)) -> Big_int_Z.compare_big_int x y
      | ((V _),(M _)) -> -1
      | ((M _),(V _)) -> 1
end

module MVS = Set.Make(MV)
module VH = Var.VarHash

let get_symbvar_value t var =
   let prev_stmt = ref (List.hd t) in
   try
      List.iter (fun s -> match s with
      | Move(_,Store(_,_,v,_,_),_) when v=var ->
            (
               match !prev_stmt with
               | Comment(_,Context{value=value}::_) -> raise (FoundValue value)
               | _ -> ()
            )
      | Comment(str,attrs) when (String.compare str "All blocks must have two statements")==0 -> ()
      | _ -> prev_stmt := s) t;
      bi0 (* XXX: Not Found == 0, for now *)
   with
   | FoundValue v -> v


let print_interesting t vars =
   dprintf "  [+] Cardinality of Set: %d" (MVS.cardinal vars);
   MVS.iter (fun k -> match k with
   | MV.V(x) -> dprintf "   [-] Var name: %s, value: 0x%s" (Pp.var_to_string x)
   (Util.hex_of_big_int (get_symbvar_value t (Var x)));
   | MV.M(x) -> dprintf "   [-] Addr: %s" (Util.hex_of_big_int x)) vars

(* add_refs:
 *    - Given an expression e, it recursively adds referenced value (use) to the
 *    global set. Note that this only gets invoked when assigned value (def) is
 *    found inside of the set of interest. Also, it checks if the expression is
 *    Load operation, we add the memory location to the set -- this is an
 *    interesting use to us.
 *
 *    - Returns the resulting set.
 *)
let add_refs vars e =
   let varvis =
      object(self)
         inherit Ast_visitor.nop

         method visit_exp e =
            match e with
            | Let (v, e1, e2) ->
              ignore(Ast_visitor.exp_accept self e1);
	      vars := MVS.add (MV.V v) !vars;
              ignore(Ast_visitor.exp_accept self e2);
	      (* This doesn't handle shadowing Lets properly, e.g.,
		 let v = 5 in let v = 4 in v *)
              vars := MVS.remove (MV.V v) !vars;
              `SkipChildren
            | Load (_,Int(addr,_),_,_) ->
               vars := MVS.add (MV.M addr) !vars;
              `DoChildren
            | _ -> `DoChildren
         method visit_rvar r =
           if not(MVS.mem (MV.V r) !vars) then
             (
	       if Typecheck.is_integer_type (Var.typ r) then (
		 vars := MVS.add (MV.V r) !vars;
	       )
             );
           `DoChildren
      end
   in
   ignore(Ast_visitor.exp_accept varvis e);
   !vars

(* is_interesting_mem_write:
 *    - Given an expression e, it tells you if there is a memory write of our
 *    interest. It does that by doing the following:
 *        + Look for Store operation and check if the destination memory address
 *        is in the interesting set. If it is, we have found 'def' case of that
 *        memory location, so we set the flag.
 *
 *    - Returns the flag, representing if we are interested in this particular
 *    expression e.
 *)
let is_interesting_mem_write vars e =
   let interesting_flag = ref false in
   let mems = ref MVS.empty in
   let memvis_one =
      object(self)
         inherit Ast_visitor.nop

         method visit_exp e =
            match e with
            | Store(_,Int(addr,_),value,_,_) ->
                  if (MVS.mem (MV.M addr) !vars) then
                     (
                        mems := MVS.add (MV.M addr) !mems;
                        interesting_flag := true;
                        `SkipChildren
                     )
                  else
                     `DoChildren
            | _ -> `DoChildren
      end
   in
   ignore(Ast_visitor.exp_accept memvis_one e);
   (!interesting_flag, !mems)

(* get_ref:
 *    - Given a list of statements, it collects all the root references of the
 *    elements in the initial set (normally, faulting operand). It will
 *    basically match the statement with Move, and updates the set based on the
 *    use/def of the variable or memory.
 *       + It follows the initial elements backwards (from the end to the
 *       beginnig) to find the root reference of those elements.
 *
 *    - Returns the resulting set. This should only contain the references that
 *    the initial elements are derived from.
 *)
let get_ref stmts fop =
   let rev_stmts = List.rev stmts in
   let vars = ref MVS.empty in
   if (fop == Disasm_i386.eip) then
      (vars := MVS.add (MV.V Disasm_i386.ra) !vars)
   else
      (vars := MVS.add (MV.V fop) !vars);
   List.iter (fun stmt ->
      (match stmt with
      | Move(l, e, a) ->
	(* If l is interesting, then any location referenced in e is
	   interesting too. *)
        if (MVS.mem (MV.V l) !vars &&
	      Typecheck.is_integer_type (Var.typ l)) then (
          vars := (MVS.remove (MV.V l) !vars);
          vars := add_refs vars e
        ) else (
	(* Alternatively, if there is a write to an interesting
	   memory location, then we should also add any referenced locations. *)
	(* XXX: Any function starting with 'is_' should not return a
	   set. *)
        let flag,mems = is_interesting_mem_write vars e in
        if(flag) then (
          vars := MVS.diff !vars mems;
          vars := add_refs vars e
        ))
      | _ -> ();
      );
   ) (rev_stmts);
   !vars

(* let apply_analyze_func exp rule = *)
(*   let analyze_func = rule.analyze_func in *)
(*    (\* Check if the exception code matches *\) *)
(*    if *)
(*    ( *)
(*       ((rule.proc_mode <> user))   (\* only handles user-mode for now *\) *)
(*       || *)
(*       ((rule.expt_addr_range <> dont_care) && *)
(*        ( ((rule.expt_addr_range == in_kernel_memory) && (isAddrUserMode exp)) || *)
(*          ((rule.expt_addr_range == in_user_memory) && (not (isAddrUserMode exp))) || *)
(*          ((rule.expt_addr_range == near_null) && (not (isAddrNearNull exp))) || *)
(*          ((rule.expt_addr_range == not_near_null) && (isAddrNearNull exp)) ) ) *)
(*       || *)
(*       ((rule.expt_subtype <> dont_care) && *)
(*        (rule.expt_subtype <> (Int64.logand (Int64.of_int exp.invalidOperation) 0xFFFFFFFFL)) ) *)
(*       || *)
(*       ((rule.expt_type <> dont_care) && *)
(*        (rule.expt_type <> (Int64.logand (Int64.of_int exp.exceptno) 0xFFFFFFFFL))) *)
(*    ) then () *)
(*    else *)
(*       ( *)
(*          if ((analyze_func exp) && rule.result_is_final) then *)
(*             ( *)
(*                report_result (rule,exp); *)
(*                raise FinalRule *)
(*             ) *)
(*       ) *)


(* (\* val get_reg: string -> Var.t *)
(*  * *)
(*  * This function gets a register name in string, and returns a register variable *)
(*  * (in Disasm_i386.regs) that corresponds to that name. *)
(*  * *)
(*  * This fails if the given register is not found in the register list. *)
(*  *\) *)
(* let get_reg r = *)
(*    let x86_regs = Disasm_i386.regs in *)
(*    try *)
(*       List.find (fun v -> (String.compare (Var.name v) r) == 0) x86_regs *)
(*    with *)
(*    | Not_found -> failwith "Unknown x86 Register!" *)


(* let isUserControllable vars = *)
(*    let isSymb = function *)
(*       | MV.V(x) -> (String.compare (String.sub (Pp.var_to_string x) 0 4) "symb") == 0 *)
(*       | MV.M(x) -> false (\* XXX: Handle Symbolic memory *\) *)
(*    in *)
(*    let controllable = *)
(*       MVS.filter (isSymb) vars *)
(*    in *)
(*    (MVS.cardinal controllable) > 0 *)


(* (\* Help function to print the stack *\) *)
(* let rec print_stack sd b t = *)
(*    match sd with *)
(*    | [] -> dprintf "Done printing Stack!" *)
(*    | x::xs -> *)
(*          if b <= t then (dprintf "Addr: %#Lx, Value: %#Lx" b x); *)
(*          print_stack xs (Int64.add b 4L) t *)


(* let check_exploitable t = *)
(*    (dprintf "[*] Concretizing..."); *)
(*    let revt = List.rev (Traces.clean t) in *)
(*    let stack_dump = *)
(*       match (List.nth revt 0) with *)
(*       | Special (_, [(StackDump s)]) -> s *)
(*       | _ -> [] *)
(*    in *)
(*    let taint_info = List.nth revt 1 in *)
(*    let expt = List.nth revt 2 in *)
(*    (\*let ct = Traces.concrete t in*\) *)
(*    (dprintf "[*] Checking exploitability..."); *)
(*    let ei = get_expt_info expt in *)
(*    ( *)
(*       try *)
(*          List.iter (apply_analyze_func ei) rules *)
(*       with *)
(*       | FinalRule -> finished := true *)
(*    ); *)

(*    (dprintf ""); *)

(*    if((not !finished) || !post_mortem) then *)
(*    ( *)
(*       ( *)
(*          if (not !finished) then *)
(*             ((\* XXX: Remove the banner. This is only for the demo purpose for now *\) *)
(*                dprintf "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="; *)
(*                dprintf "=-         Exploitability Checker Report         -="; *)
(*                dprintf "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="; *)
(*                dprintf "[*] !exploitable rules checked, but no final result found.") *)
(*          else *)
(*             (dprintf "[*] Post-Mortem analysis enabled. Performing extra analysis.") *)
(*       ); *)
(*       if ((String.compare ei.faultyOperand "None") == 0) then *)
(*          (dprintf "[*] No Faulty Operand") *)
(*       else *)
(*          ( *)
(*             let fop = get_reg ei.faultyOperand in *)
(*             (dprintf "[*] Faulty operand: %s" (Var.name fop)); *)
(*             (\*let vars = get_ref (Traces.clean ct) fop in *)

(*             (\* XXX: Do some analysis with 'vars' here *\) *)
(*             if (isUserControllable vars) then *)
(*                ( *)
(*                   dprintf " [!] These values are user-controllable. This indicates high exploitability."; *)
(*                   print_interesting ct vars; *)
(*                );*\) *)
(*          ); *)

(*       if (!post_mortem) then *)
(*       ( *)
(*          ( *)
(*             match !module_names with *)
(*             | [] -> () *)
(*             | _ -> *)
(*                   let doperand = *)
(*                      (match (List.nth revt 3) with *)
(*                      | Move (v,e,a) -> v *)
(*                      | _ -> Disasm_i386.eip (\*failwith "Not a Move"*\) *)
(*                      ) *)
(*                   in *)
(*                   let hugebin = load_modules() in *)
(*                   let pred = *)
(*                      Exploitable_symbeval.post_mortem hugebin (big_int_of_int64 ei.from_addr) taint_info (stack_dump,ei.stack_bottom,ei.stack_top) doperand *)
(*                   in *)
(*                   let formula = Memory2array.coerce_exp pred in *)
(*                   let solver = new Z3.solver in *)
(*                   solver#add_constraint formula; *)
(*                   let sat = solver#is_sat in *)
(*                   if sat then *)
(*                      (dprintf " [!!!] User-controllable data can manipulate control flow! EXPLOITABLE!!"; *)
(*                      dprintf " [DEMO] Predicate: %s" (Pp.ast_exp_to_string pred)) *)
(*                   else *)
(*                      dprintf " [...] Dangerous operation not found :)" *)

(*          ); *)
(*       ); *)

(*    ); *)
(*    t *)
