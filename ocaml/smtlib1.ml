(** Output to SMTLIB1 format
*)
open Type
open Ast
open Typecheck

module D = Debug.Make(struct let name = "STP" and default=`Debug end)
open D



module VH = Var.VarHash



(** Returns a list of free variables in the given expression *)
let freevars e =
  let freevis =
    object(self)
      inherit Ast_visitor.nop
      val ctx = VH.create 570
      val found = VH.create 570

      method get_found =
	dprintf "found %d freevars" (VH.length found);
	VH.fold (fun k () a -> k::a) found []
      method add_dec d = 
	if not(VH.mem found d || VH.mem ctx d)
	then VH.add found d ()
	else dprintf "Not adding %s." (Pp.var_to_string d)

      method visit_exp = function
	| Let(v, e1, e2) -> 
	    ignore(Ast_visitor.exp_accept self e1);
	    VH.add ctx v ();
	    ignore(Ast_visitor.exp_accept self e2);
	    VH.remove ctx v;
	    `SkipChildren
	| _ ->
	    `DoChildren
	      
      method visit_rvar r =
	self#add_dec r;
	`DoChildren
    end
  in
  ignore(Ast_visitor.exp_accept freevis e);
  freevis#get_found

(** Returns a list of free variables in the given expression *)
let myfreevars e =
  let ctx = VH.create 570 
  and found = VH.create 570 in
  let get_found () =
    dprintf "found %d freevars" (VH.length found);
    VH.fold (fun k () a -> k::a) found []
  in
  let add_dec d = 
    if not(VH.mem found d || VH.mem ctx d)
    then VH.add found d ()
    else dprintf "Not adding %s." (Pp.var_to_string d)
  in
  let rec freevis = function
    | Let(v, e1, e2) -> freevis e1; VH.add ctx v (); freevis e2; VH.remove ctx v
    | Var v -> add_dec v
	(* Visit all children *)
    | Load (e1,e2,e3,_) -> freevis e1; freevis e2; freevis e3
    | Store (e1,e2,e3,e4,_) -> freevis e1; freevis e2; freevis e3; freevis e4
    | BinOp (_,e1,e2) -> freevis e1; freevis e2
    | UnOp (_, e) -> freevis e
    | Cast (_,_,e) -> freevis e
    | _ -> ()
  in
  freevis e;
  get_found ()

class pp ?suffix:(s="") ft =
  let pp = Format.pp_print_string ft
  and pc = Format.pp_print_char ft
  and pi = Format.pp_print_int ft
  and space = Format.pp_print_space ft
(*  and cut = Format.pp_print_cut ft*)
  and force_newline = Format.pp_force_newline ft
  and printf f = Format.fprintf ft f
  and opn  = Format.pp_open_box ft
  and flush = Format.pp_print_flush ft
  and cls = Format.pp_close_box ft in
  let var2s (Var.V(num,name,_)) =
    name^"_"^(string_of_int num)^s
  in


object (self)

  val used_vars : (string,Var.t) Hashtbl.t = Hashtbl.create 57
  val ctx : string VH.t = VH.create 57
    
  val mutable unknown_counter = 0;

  val mutable let_counter = 0;

  method flush () =
    flush();

  method extend v s =
    assert(not(Hashtbl.mem used_vars s));
    Hashtbl.add used_vars s v;
    VH.add ctx v s

  method unextend v =
    VH.remove ctx v

  method var v =
    try 
      pp (VH.find ctx v)
    with Not_found ->
      let s = var2s v in
      self#extend v s; (* FIXME: is this really what we want? *)
      pp s

  method varname v =
    VH.find ctx v

  method declare_new_freevars e =
    opn 1;
    pp "; free variables:"; force_newline();
    let fvs = myfreevars e in 
    List.iter (fun v -> if not(VH.mem ctx v) then self#decl v) fvs;
    cls ();
    pp "; end free variables."; 
    force_newline();
    flush()
       
  method typ = function
    | Reg n ->	printf "BitVec[%u]" n
    | Array(Reg idx, Reg elmt) -> printf "Array[%u:%u] " idx elmt;
    | Array _ -> failwith "SMTLIB1 only supports Arrays with register indices and elements"
    | TMem _ ->	failwith "TMem unsupported by SMTLIB1"


  method decl (Var.V(_,_,t) as v) =
    self#extend v (var2s v);
    pp ":extrafuns (("; self#var v; space (); self#typ t; pp "))"; force_newline();



  method ast_exp e =
    opn 0;
    (match e with
     | Int((i, Reg t) as p) ->
	 let maskedval = Arithmetic.to64 p in
	 pp "bv"; pp (Int64.to_string maskedval); pp "["; pi t; pp "]";
     | Int _ -> failwith "Ints may only have register types"
     | Var v ->
	 self#var v
     | UnOp(uop, o) ->
	 (match uop with
	  | NEG -> pp "(bvneg"; space ();
	  | NOT -> pp "(bvnot"; space ();
	 );
	 self#ast_exp o;
	 pc ')'
     (* 	   (\* Eww, the << operator in stp wants a constant int on the right, *)
     (* 	      rather than a bitvector *\) *)
     (* | BinOp((LSHIFT|RSHIFT|ARSHIFT), e1, Int(i,_)) when i = 0L -> *)
     (* 	 self#ast_exp e1 *)
     (* | BinOp(LSHIFT, e1, Int(i,_)) -> *)
     (* 	 let  t = infer_ast ~check:false e1 in *)
     (* 	 pp "(("; self#ast_exp e1; pp" << "; pp (Int64.to_string i); pp ")["; *)
     (* 	 pp (string_of_int(bits_of_width t - 1)); pp":0])" *)
     (* | BinOp(RSHIFT, e1, Int(i,_)) -> (\* Same sort of deal :( *\) *)
     (* 	 pc '('; self#ast_exp e1; pp " >> "; pp(Int64.to_string i); pc ')' *)
     (* | BinOp(ARSHIFT, e1, Int(i,_)) -> (\* Same sort of deal :( *\) *)
     (* 	 let t = infer_ast ~check:false e1 in *)
     (* 	 let bits = string_of_int (bits_of_width t) in *)
     (* 	 let gethigh = Int64.sub (Int64.of_int (bits_of_width t)) i in *)
     (* 	 let gethigh = Int64.sub gethigh 1L in *)
     (* 	 if gethigh >= 0L then ( *)
     (* 	   pp "SX(("; self#ast_exp e1;  *)
     (* 	   pp " >> "; pp (Int64.to_string i); *)
     (* 	   pp ")["; pp (Int64.to_string gethigh); pp ":0], "; pp bits; pc ')' *)
     (* 	 ) else ( *)
     (* 	   let b = Int64.sub (Int64.of_int (bits_of_width t)) 1L in *)
     (* 	   pp "SX("; self#ast_exp e1; pp "["; pp (Int64.to_string b); *)
     (* 	   pp ":"; pp (Int64.to_string b); pp "], "; pp bits; pc ')'; *)
     (* 	 ) *)
     | BinOp(NEQ, e1, e2) ->
	 (* Rewrite NEQ in terms of EQ *)
       let newe = UnOp(NOT, BinOp(EQ, e1, e2)) in
       self#ast_exp newe
     | BinOp((EQ|LT|LE|SLT|SLE) as op, e1, e2) ->
       (* These are predicates, which return boolean values. We need
	  to convert these to one-bit bitvectors. *)
       let f = match op with
	 | EQ -> "="
	 | LT -> "bvult"
	 | LE -> "bvule"
	 | SLT -> "bvslt"
	 | SLE -> "bvsle"
	 | _ -> assert false
       in
       pp "(ite (";
       pp f;
       space ();
       self#ast_exp e1;
       space ();
       self#ast_exp e2;
       pp ") bv1[1] bv0[1])";
     | BinOp((PLUS|MINUS|TIMES|DIVIDE|SDIVIDE|MOD|SMOD|AND|OR|XOR|LSHIFT|RSHIFT|ARSHIFT) as bop, e1, e2) as e ->
	 let t = infer_ast ~check:false e1 in
	 let t' = infer_ast ~check:false e2 in
	 if t <> t' then
	   wprintf "Type mismatch: %s" (Pp.ast_exp_to_string e);
	 assert (t = t') ;
	 let fname = match bop with
	   | PLUS -> "bvadd"
	   | MINUS -> "bvsub"
	   | TIMES -> "bvmul"
	   | DIVIDE -> "bvudiv"
	   | SDIVIDE -> "bvsdiv"
	   | MOD -> "bvurem"
	   | SMOD -> "bvsrem"
	   | AND -> "bvand"
	   | OR -> "bvor"
	   | XOR -> "bvxor"
	   | EQ|NEQ|LE|LT|SLT|SLE -> assert false
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
	    | CAST_LOW      -> ("(extract["^string_of_int(bitsnew-1)^":0] ", ")")
	    | CAST_HIGH     -> ("(extract["^string_of_int(bitsold-1)^":"^string_of_int(bitsold-bitsnew)^"] ", ")")
	    | CAST_UNSIGNED -> ("(zero_extend["^string_of_int(delta)^"]", ")")
	    | CAST_SIGNED -> ("(sign_extend["^string_of_int(delta)^"]", ")")
	  in
	  pp pre;
	  self#ast_exp e1;
	  space ();
	  pp post
      | Unknown(s,t) ->
	  pp "unknown_"; pi unknown_counter; pp" ;"; pp s; force_newline();
	  unknown_counter <- unknown_counter + 1;
      | Lab lab ->
	  failwith ("SMTLIB: don't know how to handle label names: "
		      ^ (Pp.ast_exp_to_string e))
      | Let(v, e1, e2) ->
	  pp "(let (";
	  (* v isn't allowed to shadow anything. also, smtlib requires it be prefixed with ? *)
	  let s = "?" ^ var2s v ^"_"^ string_of_int let_counter in
	  let_counter <- succ let_counter;
	  pp s;
	  opn 2; space();
	  self#ast_exp e1;
	  pc ')';
	  space(); cls();
	  space();
	  self#extend v s;
	  self#ast_exp e2;
	  self#unextend v;
	  pc ')'
      | Load(arr,idx,endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  pp "(select ";
	  self#ast_exp arr;
	  space ();
	  self#ast_exp idx;
	  space ();
	  pc ')'
      | Store(arr,idx,vl, endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  pp "(store ";
	  self#ast_exp arr;
	  space ();
	  self#ast_exp idx;
	  space ();
	  self#ast_exp vl;
	  pc ')'
    );
    cls();



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
    pp ":assumption (=";
    space();
    self#forall foralls;
    pp "bv1[1] ";
    force_newline();
    self#ast_exp e;
    force_newline();
    pp ")";
    force_newline ();
    self#counterexample ();
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
    pp "(= bv1[1] ";
    force_newline();
    self#ast_exp e;
    force_newline();
    pp "));";
    self#close_benchmark ()

  method counterexample () =
    pp ":formula true";
    force_newline();

  method close =
    Format.pp_print_newline ft ();

end


class pp_oc ?suffix:(s="") fd =
  let ft = Format.formatter_of_out_channel fd in
object
  inherit pp ~suffix:s ft as super
  method close =
    super#close;
    close_out fd
end

