(** Output to SMTLIB1 format *)
open Type
open Ast
open Ast_convenience
open Typecheck

module D = Debug.Make(struct let name = "smtlib1" and default=`Debug end)
open D

exception No_rule

module VH = Var.VarHash

type sort = BitVec | Bool

class pp ?suffix:(s="") ft =
  let pp = Format.pp_print_string ft
  and pc = Format.pp_print_char ft
  and pi = Format.pp_print_int ft
  and space = Format.pp_print_space ft
  and cut = Format.pp_print_cut ft
  and force_newline = Format.pp_force_newline ft
  and printf f = Format.fprintf ft f
  and opn  = Format.pp_open_box ft
  and flush = Format.pp_print_flush ft
  and cls = Format.pp_close_box ft in
  let var2s (Var.V(num,name,_)) =
    name^"_"^(string_of_int num)^s
  in

  let opflatten e =
    let rec oh bop e1 e2 =
      let l1 = match e1 with
	| BinOp(bop', e'1, e'2) when bop' = bop ->
	    oh bop e'1 e'2
	| _ -> [e1]
      in
      let l2 = match e2 with
	| BinOp(bop', e'1, e'2) when bop' = bop ->
	    oh bop e'1 e'2
	| _ -> [e2]
      in
      Util.fast_append l1 l2
    in
    match e with
    | BinOp(bop, e1, e2) ->
	oh bop e1 e2
    | _ -> failwith "opflatten expects a binop"
  in

object (self)
  inherit Formulap.fpp
  val used_vars : (string,Var.t) Hashtbl.t = Hashtbl.create 57
  val ctx : (string*sort) VH.t = VH.create 57
    
  val mutable unknown_counter = 0;

  val mutable let_counter = 0;

  method bool_to_bv e =
    pp "(ite";
    space ();
    self#ast_exp_bool_base e;
    space ();
    self#ast_exp_base exp_true;
    space ();
    self#ast_exp_base exp_false;
    pc ')'

  method bv_to_bool e =
    pp "(=";
    space ();
    pp "bv1[1]";
    space ();
    self#ast_exp_base e;
    pp ")"

  method flush () =
    flush();

  method extend v s st =
    assert(not(Hashtbl.mem used_vars s));
    Hashtbl.add used_vars s v;
    VH.add ctx v (s,st)

  method unextend v =
    VH.remove ctx v

  method var v =
    match (VH.find ctx v) with
    | n,_ -> pp n

  method letme v e1 e2 st =
    let t1 = Typecheck.infer_ast e1 in
    let cmd,c,pf,vst = match t1 with Reg 1 -> "flet","$",self#ast_exp_bool,Bool | _ -> "let","?",self#ast_exp,BitVec in
    let pf2 = match st with Bool -> self#ast_exp_bool | BitVec -> self#ast_exp in
    pp "("; pp cmd; pp " (";
    (* v isn't allowed to shadow anything. also, smtlib requires it be prefixed with ? or $ *)
    let s = c ^ var2s v ^"_"^ string_of_int let_counter in
    let_counter <- succ let_counter;
    pp s;
    pc ' ';
    pf e1;
    pc ')';
    space ();
    self#extend v s vst;
    pf2 e2;
    self#unextend v;
    cut ();
    pc ')'

  method varname v =
    match VH.find ctx v with
    | n,_ -> n

  method varsort v =
    match VH.find ctx v with
    | _,st -> st

  method declare_new_freevars e =
    force_newline();
    pp "; free variables:"; force_newline();
    let fvs = Formulap.freevars e in 
    List.iter (fun v -> if not(VH.mem ctx v) then self#decl v) fvs;
    pp "; end free variables."; 
    force_newline()
       
  method typ = function
    | Reg n ->	printf "BitVec[%u]" n
    | Array(Reg idx, Reg elmt) -> printf "Array[%u:%u] " idx elmt;
    | Array _ -> failwith "SMTLIB1 only supports Arrays with register indices and elements"
    | TMem _ ->	failwith "TMem unsupported by SMTLIB1"

  method decl (Var.V(_,_,t) as v) =
    let sort = match t with
      (* | Reg 1 -> Bool (\* Let's try making all 1-bit bvs bools for now *\) *)
      | _ -> BitVec
    in
    self#extend v (var2s v) sort;
    pp ":extrafuns (("; self#var v; space (); self#typ t; pp "))"; force_newline();

  method ast_exp_base e =
    opn 0;
    (* let t = Typecheck.infer_ast e in *)
    (match e with
     | Int((i, Reg t) as p) ->
	 let maskedval = Arithmetic.to64 p in
	 pp "bv"; printf "%Lu" maskedval; pp "["; pi t; pp "]";
     | Int _ -> failwith "Ints may only have register types"
     | Var v ->
	 let name,st = VH.find ctx v in
	 (match st with 
	 | BitVec -> pp name;
	 | Bool -> raise No_rule)
     | UnOp(uop, o) ->
	 (match uop with
	  | NEG -> pp "(bvneg"; space ();
	  | NOT -> pp "(bvnot"; space ();
	 );
	 self#ast_exp o;
	 pc ')'
     | BinOp((AND|OR), _, _) when parse_ite e <> None ->
	 let b, e1, e2 = match parse_ite e with
	   | Some(b, e1, e2) -> b, e1, e2
	   | None -> assert false
	 in
	 pp "(ite";
	 space ();
	 self#ast_exp_bool b;
	 space ();
	 self#ast_exp e1;
	 space ();
	 self#ast_exp e2;
	 cut ();
	 pc ')';
     | BinOp((PLUS|MINUS|TIMES|DIVIDE|SDIVIDE|MOD|SMOD|AND|OR|XOR|LSHIFT|RSHIFT|ARSHIFT) as bop, e1, e2) as e ->
	 let t = infer_ast ~check:false e1 in
	 let t' = infer_ast ~check:false e2 in
	 if t <> t' then
	   wprintf "Type mismatch: %s" (Pp.ast_exp_to_string e);
	 assert (t = t') ;
	 let fname = match bop with
	   (* | EQ -> "bvcomp" *) (* bvcomp doesn't work on memories! *)
	   | PLUS -> "bvadd"
	   | MINUS -> "bvsub"
	   | TIMES -> "bvmul"
	   | DIVIDE -> "bvudiv"
	   | SDIVIDE -> "bvsdiv"
	   | MOD -> "bvurem"
	   (* | SMOD -> "bvsrem" *)
	   | SMOD -> failwith "SMOD goes to bvsrem or bvsmod?"
	   | AND -> "bvand"
	   | OR -> "bvor"
	   | XOR -> "bvxor"
	   | NEQ|EQ|LE|LT|SLT|SLE -> assert false
	   | LSHIFT -> "bvshl"
	   | RSHIFT -> "bvlshr"
	   | ARSHIFT -> "bvashr"
	 in
	 pc '('; pp fname; space (); self#ast_exp e1; space (); self#ast_exp e2; pc ')';
     | Cast((CAST_LOW|CAST_HIGH|CAST_UNSIGNED|CAST_SIGNED) as ct,t, e1) ->
	  let t1 = infer_ast ~check:false e1 in
	  let (bitsnew, bitsold) = (bits_of_width t, bits_of_width t1) in
	  let delta = bitsnew - bitsold in
	  (match ct with
	    | CAST_LOW | CAST_HIGH -> assert (delta <= 0);
	    | CAST_UNSIGNED | CAST_SIGNED -> assert (delta >= 0));
	  let (pre,post) = match ct with
	    | _ when bitsnew = bitsold -> ("","")
	    | CAST_LOW      -> ("(extract["^string_of_int(bitsnew-1)^":0]", ")")
	    | CAST_HIGH     -> ("(extract["^string_of_int(bitsold-1)^":"^string_of_int(bitsold-bitsnew)^"]", ")")
	    | CAST_UNSIGNED -> ("(zero_extend["^string_of_int(delta)^"]", ")")
	    | CAST_SIGNED -> ("(sign_extend["^string_of_int(delta)^"]", ")")
	  in
	  pp pre;
	  space ();
	  self#ast_exp e1;
	  cut ();
	  pp post
      | Unknown(s,t) ->
	  pp "unknown_"; pi unknown_counter; pp" ;"; pp s; force_newline();
	  unknown_counter <- unknown_counter + 1;
      | Lab lab ->
	  failwith ("SMTLIB: don't know how to handle label names: "
		      ^ (Pp.ast_exp_to_string e))
      | Let(v, e1, e2) -> self#letme v e1 e2 BitVec
      | Load(arr,idx,endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  pp "(select ";
	  self#ast_exp arr;
	  space ();
	  self#ast_exp idx;
	  cut ();
	  pc ')'
      | Store(arr,idx,vl, endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  pp "(store ";
	  self#ast_exp arr;
	  space ();
	  self#ast_exp idx;
	  space ();
	  self#ast_exp vl;
	  cut ();
	  pc ')'
      | _ -> raise No_rule
    );
    cut();
    cls()

  (** Evaluate an expression to a bitvector *)
  method ast_exp e =
    let t = Typecheck.infer_ast e in
    if t = reg_1 then
      try
	self#bool_to_bv e
      with No_rule ->
	self#ast_exp_base e
    else
      self#ast_exp_base e

  (** Try to evaluate an expression to a boolean. If no good rule
      exists, then raises the No_rule exception. *)
  method ast_exp_bool_base e =
    let t = Typecheck.infer_ast e in
    assert (t = reg_1);
    opn 0;
    (match e with
     | Int((i, Reg t) as p) when t = 1 ->
	 let maskedval = Arithmetic.to64 p in
	 (match maskedval with
	  | 0L -> pp "false"
	  | 1L -> pp "true"
	  | _ -> failwith "ast_exp_bool")
     | Int((i, Reg t)) -> failwith "ast_exp_bool only takes reg_1 expressions"
     | Int _ -> failwith "Ints may only have register types"
     | UnOp((NEG|NOT), o) ->
	 (* neg and not are the same for one bit! *)
	 pp "(not";
	 space ();
	 self#ast_exp_bool o;
	 cut ();
	 pc ')'
     | BinOp(NEQ, e1, e2) ->
	 (* Rewrite NEQ in terms of EQ *)
       let newe = UnOp(NOT, BinOp(EQ, e1, e2)) in
       self#ast_exp_bool newe
     | BinOp((OR|AND), _, _) when parse_ite e <> None ->
	 let b, e1, e2 = match parse_ite e with
	   | Some(b, e1, e2) -> b, e1, e2
	   | None -> assert false
	 in
	 pp "(if_then_else";
	 space ();
	 self#ast_exp_bool b;
	 space ();
	 self#ast_exp_bool e1;
	 space ();
	 self#ast_exp_bool e2;
	 cut ();
	 pc ')';
     | BinOp((EQ|LT|LE|SLT|SLE) as op, e1, e2) ->
       (* These are predicates, which return boolean values. We need
	  to convert these to one-bit bitvectors. *)
       let t1 = Typecheck.infer_ast e1 in
       let t2 = Typecheck.infer_ast e2 in
       assert (t1 = t2);
       let f = match op with
	 | EQ when t1 = reg_1 -> "iff" (* = only applies to terms... but booleans are formulas, not terms *)
	 | EQ -> "="
	 | LT -> "bvult"
	 | LE -> "bvule"
	 | SLT -> "bvslt"
	 | SLE -> "bvsle"
	 | _ -> assert false
       in
       let pf = if t1 = reg_1 then self#ast_exp_bool else self#ast_exp
       in
       pp "(";
       pp f;
       space ();
       pf e1;
       space ();
       pf e2;
       pp ")";
       cut ();
     | BinOp((AND|OR|XOR) as bop, e1, e2) ->
    	 let t = infer_ast ~check:false e1 in
    	 let t' = infer_ast ~check:false e2 in
    	 if t <> t' then
    	   wprintf "Type mismatch: %s" (Pp.ast_exp_to_string e);
    	 assert (t = t') ;
    	 let fname = match bop with
    	   | AND -> "and"
    	   | OR -> "or"
    	   | XOR -> "xor"
	   | _ -> assert false
    	 in
	 let oplist = opflatten e in
    	 (* pc '('; pp fname; space (); self#ast_exp_bool e1; space (); self#ast_exp_bool e2; pc ')'; *)
    	 pc '('; pp fname; 
	 List.iter
	   (fun e -> space (); self#ast_exp_bool e) oplist;
	 pc ')';
     | Cast((CAST_LOW|CAST_HIGH|CAST_UNSIGNED|CAST_SIGNED),t, e1) ->
     	  let t1 = infer_ast ~check:false e1 in
     	  let (bitsnew, bitsold) = (bits_of_width t, bits_of_width t1) in
     	  let delta = bitsnew - bitsold in
     	  if delta = 0 then self#ast_exp_bool e1 else self#bv_to_bool e
     | Var v ->
	 let name,st = VH.find ctx v in
	 (match st with
	 | BitVec -> self#bv_to_bool e
	 | Bool -> pp name) 
     | Let(v, e1, e2) -> self#letme v e1 e2 Bool
     | _ -> cls(); raise No_rule
    );
    cut();
    cls()

  (** Try to evaluate an expression to a boolean. If no good rule
      exists, uses bitvector conversion instead. *)
  method ast_exp_bool e =
    let t = Typecheck.infer_ast e in
    assert (t = reg_1);
    try self#ast_exp_bool_base e
    with No_rule ->
      self#bv_to_bool e


  method forall = function
    | [] -> ()
    | v::vars ->
	let var_type  (Var.V(_,_,t) as v) =
	  self#var v; pp " : "; self#typ t
	in
	opn 2;
	pp "FORALL (";space();
	(* TODO: group by type *)
	List.iter (fun v -> var_type v; pc ','; space()) vars;
	var_type v;
	pp "):";
	cls();space();

  method exists = function
    | [] -> ()
    | v::vars ->
	let var_type  (Var.V(_,_,t) as v) =
	  self#var v; pp " : "; self#typ t
	in
	opn 2;
	pp "EXISTS (";space();
	  (* TODO: group by type *)
	List.iter (fun v -> var_type v; pc ','; space()) vars;
	var_type v;
	pp "):";
	cls();space();

  method open_benchmark () =
    pc '(';
    opn 0;
    pp "benchmark file.smt";
    force_newline();
    pp ":status unknown";
    force_newline();
    pp ":source { Source Unknown }";
    force_newline();
    pp ":difficulty { Unknown }";
    force_newline();
    pp ":category { Unknown }";
    force_newline();
    pp ":logic QF_AUFBV";
    force_newline()

  method close_benchmark () =
    pc ')'; cls()

  (* method assert_eq v e = *)
  (*   opn 0; *)
  (*   self#declare_new_freevars (BinOp(EQ, Var v, e)); *)
  (*   force_newline(); *)
  (*   pp ":assumption (="; *)
  (*   self#var v; *)
  (*   space (); *)
  (*   self#ast_exp e; *)
  (*   pc ')'; *)
  (*   cls() *)

  method assert_ast_exp_with_foralls ?(fvars=true) foralls e =
    self#open_benchmark ();
    if fvars then (
      self#declare_new_freevars e;
      force_newline();
    );
    opn 1;
    pp ":assumption";
    space();
    self#forall foralls;
    self#ast_exp_bool e;
    cut();
    cls();
    force_newline ();
    self#formula ();
    self#close_benchmark ()

  method assert_ast_exp e =
    self#assert_ast_exp_with_foralls [] e

  (** Is e a valid expression (always true)? *)
  method valid_ast_exp ?(exists=[]) ?(foralls=[]) e =
    self#open_benchmark ();
    self#declare_new_freevars e;
    force_newline();
    pp ":formula (";
    self#exists exists;
    self#forall foralls;
    self#ast_exp_bool e;
    pp ");";
    self#close_benchmark ()

  (* (\** Is e a valid expression (always true)? *\) *)
  (* method valid_ast_exp ?(exists=[]) ?(foralls=[]) e = *)
  (*   self#open_benchmark (); *)
  (*   self#declare_new_freevars e; *)
  (*   force_newline(); *)
  (*   pp ":formula ("; *)
  (*   self#exists exists; *)
  (*   self#forall foralls; *)
  (*   pp "(= bv1[1] "; *)
  (*   force_newline(); *)
  (*   self#ast_exp e; *)
  (*   force_newline(); *)
  (*   pp "));"; *)
  (*   self#close_benchmark () *)


  method formula () =
    pp ":formula true";
    force_newline();

  method counterexample () = ()

  method close =
    Format.pp_print_newline ft ();

end


class pp_oc ?suffix:(s="") fd =
  let ft = Format.formatter_of_out_channel fd in
object
  inherit pp ~suffix:s ft as super
  inherit Formulap.fpp_oc
  method close =
    super#close;
    close_out fd
end

