(* A module to perform trace analysis *)

open Symbeval
open Type
open Ast

module D = Debug.Make(struct let name = "TraceEval" and default=`Debug end)
open D

  let concrete: (string, Ast.exp) Hashtbl.t = Hashtbl.create 5 
  let concrete_mem: (int64, Ast.exp) Hashtbl.t = Hashtbl.create 5

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

module Trace = Symbeval.Make(TaintLookup)(PartialSubst)

(* Useful shorthands to manipulate Taint attributes *)
 let keep_taint = function 
  | Context _ -> true 
  | _ -> false 

 let unwrap_taint = function 
  | Context c -> c 
  | _ -> failwith "trying to unwrap a non-taint attribute"

(* keeping only the attributes that contain taint info *)
 let filter_taint atts = 
  let atts = List.filter keep_taint atts in
  List.map unwrap_taint atts

(* Concretely evaluating the traces to verify that the 
 * lifted form is a faithful representation of the
 * original program                                     *)

(* A default value *)
 let def = {name=""; mem=true; t=reg_1; index=0L; value=0L; taint=Untaint}
(* Values for the R_GDT and R_LDT registers -- currently we are
 * unable to recover these values from the traces               *)
 let gdt = {name="R_GDT"; mem=false; t=reg_32; index=0L; value=100L; taint=Untaint}
 let ldt = {name="R_LDT"; mem=false; t=reg_32; index=0L; value=220L; taint=Untaint}

 let eval state = 
  try
   let stmt = Trace.inst_fetch state.sigma state.pc in
   (*pdebug (Pp.ast_stmt_to_string stmt) ; *)
   let typ_to_bytes = function 
    |Reg bits -> (bits+7)/8 
    | _ -> failwith "not a register" 
   in
   let get_byte i v = Int64.logand (Int64.shift_right v ((i-1)*8)) 0xffL in
   let first_bind_to_ctx hash index value =
    if Hashtbl.mem hash index then ()
    else Hashtbl.add hash index value
   in
   let rec add_mem index value limit = function
    | 0 -> ()
    | n -> let byte = get_byte (limit-n+1) value in
           first_bind_to_ctx concrete_mem index (Int(byte,reg_8)) ;
           add_mem (Int64.succ index) value limit (n-1)
   in
   let add_to_conc {name=name; mem=mem; index=index; 
                    value=value; t=typ; taint=taint} =
    if taint = Untaint then
     if mem then
      let limit = typ_to_bytes typ in
      add_mem index value limit limit 
     else
     first_bind_to_ctx concrete name (Int(value,typ))
    in
    let init_taint_tables () =
     Hashtbl.clear concrete;
     Hashtbl.clear concrete_mem;
     add_to_conc gdt;
     add_to_conc ldt
    in
    (match stmt with
     | Label (_,atts) -> 
       let taint_atts = filter_taint atts in
       if taint_atts != [] then init_taint_tables ();
       List.iter add_to_conc taint_atts
     | _ -> ()
    );
    pdebug (string_of_int state.pc) ;
    pdebug (Pp.ast_stmt_to_string stmt) ;
    Trace.eval_stmt state stmt
  with 
  | Failure fail -> 
    (try
      let prev = Trace.inst_fetch state.sigma (pred state.pc) in
      failwith (fail^"\nprev: "^(Pp.ast_stmt_to_string prev))
     with Failure _ -> failwith (fail^" no prev"))
  | Halted _ -> 
    pdebug "trace successfully evaluated" ;
    [] (* no more states to explore *)
  
 let rec unbounded_dfs st = 
  List.iter unbounded_dfs (eval st)

(* Canonicalizing the traces and detecting the time of attack *)

 (*  Set of Assumptions:
  *
  *  - All instruction-internal jmps use labels  *
  *    of the form: L_<integer>                  *)
  let is_internal_label s = s.[0] = 'L'
 (*  - All address labels start with "pc_0x"     *)
  let is_addr_label l = 
    String.length l > 5 && String.sub l 0 5 = "pc_0x"
   
  let rec get_next_address = function
   | [] -> raise Not_found
   | (Ast.Label ((Addr n),_))::_ -> 
      Name ("pc_"^(Int64.format "0x%Lx" n))
   | _::xs -> get_next_address xs

  (* converting cjmps to asserts whenever possible *)
  let cjmps_to_asserts = 
   let rec cjmps_to_asserts acc = function
    | [] -> List.rev acc
    | (Ast.CJmp (e,l1,l2,atts) as x)::xs ->
     let newstmt = match lab_of_exp l1 with
     | Some (Name s as a) when not (is_internal_label s) ->
       let true_branch = [Ast.Jmp(l1,[]) ; Ast.Assert(e,atts)] in
       let false_branch = [Ast.Jmp(l2,[]) ; Ast.Assert(UnOp(NOT,e),atts)] in
       (try if get_next_address xs = a then true_branch
            else false_branch
        with Not_found -> false_branch
       )
     | _ -> [x]
     in
     cjmps_to_asserts (newstmt@acc) xs
    | x::xs -> cjmps_to_asserts (x::acc) xs
   in
   cjmps_to_asserts []

  (* Renames labels to avoid duplicates by appending 
   * a number to the end of the label name            *)
  let fix_labels stmts = 
    let counter = ref 0 in
    let is_addr = function
     | Name s -> is_addr_label s
     | Addr a -> false
    in
    let modify = function
     | Name s -> (s^"_"^(string_of_int !counter))
     | _ -> failwith "not a valid label"
    in
    (* rename stmt labels *)
    let subst_lab stmt = function
     | Some (Name subst) ->
      let vis = object(self)
       inherit Ast_visitor.nop
       method visit_exp = function
  	| Ast.Lab(l) when l = subst ->  
          `ChangeTo (Ast.Lab(modify (Name(l))))
  	| _ -> `DoChildren
       end
      in
      Ast_visitor.stmt_accept vis stmt
     | _ -> stmt
    in
    let rec ch_labels acc last = function
     | [] -> acc
     | (Ast.Label (l,a))::rest when is_addr l ->
       counter := !counter + 1;
       let l' = Name (modify l) in
       ch_labels ((Ast.Label (l',a))::acc) (Some l) rest
     | ((Ast.Jmp (l,a)) as jump)::rest ->
       (match last, l with
        | Some (Name subst), Lab lab 
         when is_addr_label lab ->
           if (lab <> subst) then ch_labels acc last rest
           else ch_labels ((subst_lab jump last)::acc) last rest
        | _ -> ch_labels (jump::acc) last rest
       )
     | s::rest ->
       let s = subst_lab s last in
       ch_labels (s::acc) last rest
    in
    ch_labels [] None (List.rev stmts)

 let remove_specials =
  let no_specials = function 
   | Ast.Special _ -> false 
   | _ -> true
  in
  List.filter no_specials


 let canonicalize trace = 
  let trace = cjmps_to_asserts trace in
  let trace = fix_labels trace in
  let trace = remove_specials trace in
  trace

  let detect_attack stmts =
   let rec get_first_atts = function
    | [] -> failwith "no taint analysis info were found in the trace"
    | (Ast.Label (_,atts))::rest ->
       let taint_atts = filter_taint atts in
       if taint_atts <> [] then (taint_atts, rest)
       else get_first_atts rest
    | s::rest -> get_first_atts rest 
   in
   let rec get_tainted_mem = function 
    | [] -> failwith "no tainted memory lookup was found in attributes"
    | {name=name; mem=mem; index=index; value=value}::rest ->
      if mem then (name,index,value)
      else get_tainted_mem rest
   in
   (* find when this specific memory address was overwritten *)
   let rec detect_attack last ((name,index,value) as triple) = function
    | [] -> failwith "the time of attack was not detected"
    | ((Ast.Label (_,atts))::rest) as r when atts <> [] -> 
      let atts = filter_taint atts in
      let rec found_mem  = function
       | [] -> false
       | {name=n; mem=m; index=i; value=v}::next ->
         if m && n = name && index=i && (value<>v) then true
         else found_mem next
      in
      if found_mem atts then (List.rev last) @ r
      else detect_attack [] triple rest
    | s::rest -> detect_attack (s::last) triple rest
   in
   let revstmts = List.rev stmts in
   let atts,revstmts = get_first_atts revstmts in
   let tainted_mem = get_tainted_mem atts in
   let shortened = detect_attack [] tainted_mem revstmts in
   List.rev shortened

(* Symbolically do a dfs evaluation of the trace *)
 let dfs_ast_program p = 
  let no_jumps = remove_specials p in
  let no_cjumps = cjmps_to_asserts no_jumps in
  (* Append a halt true at the end *)
  let trace = no_cjumps@[Ast.Halt (exp_true, [])] in
  let ctx = Symbolic.build_default_context trace in
  unbounded_dfs ctx



