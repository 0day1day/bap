(** Output to STP format (same as CVCL or CVC3)
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


class pp ft =
  let pp = Format.pp_print_string ft
  and pc = Format.pp_print_char ft
  and pi = Format.pp_print_int ft
  and space = Format.pp_print_space ft
  and cut = Format.pp_print_cut ft
  and force_newline = Format.pp_force_newline ft
  and printf f = Format.fprintf ft f
  and opn  = Format.pp_open_box ft
  and cls = Format.pp_close_box ft in
  let var2s (Var.V(num,name,_)) =
    name^"_"^string_of_int num
  in

object (self)

  val used_vars : (string,Var.t) Hashtbl.t = Hashtbl.create 57
  val ctx : string VH.t = VH.create 57
    
  val mutable unknown_counter = 0;

  val mutable let_counter = 0;

  method extend v s =
    assert(not(Hashtbl.mem used_vars s));
    Hashtbl.add used_vars s v;
    VH.add ctx v s

  method unextend v =
    VH.remove ctx v

  method var v =
    try pp (VH.find ctx v)
    with Not_found ->
      let s = var2s v in
      self#extend v s; (* FIXME: is this really what we want? *)
      pp s


  method declare_new_freevars e =
    opn 0;
    pp "% free variables:"; force_newline();
    let fvs = freevars e in 
    List.iter (fun v -> if not(VH.mem ctx v) then self#decl v) fvs;
    pp "% end free variables."; force_newline();
    cls()
  

  method typ = function
    | Reg n ->	printf "BITVECTOR(%u)" n
    | Array(idx,elmt) -> pp "ARRAY "; self#typ idx; pp " OF "; self#typ elmt
    | TMem _ ->	failwith "TMem unsupported by STP"


  method decl (Var.V(_,_,t) as v) =
    self#extend v (var2s v);
    self#var v; pp " : "; self#typ t; pp ";"; space()



  method ast_exp e =
    opn 0;
    (match e with
     | Int(i,t) ->
	 let format = match t with
	   | Reg 1  -> format_of_string "0bin%Ld"
	   | Reg 8  -> format_of_string "0hex%02Lx"
	   | Reg 16 -> format_of_string "0hex%04Lx"
	   | Reg 32 -> format_of_string "0hex%08Lx"
	   | Reg 64 -> format_of_string "0hex%016Lx"
	   | Reg _ -> failwith "unimplemented bitvector length"
	   | _ -> invalid_arg "Only constant integers supported"
	 in
	 let maskedval = Int64.logand i
	   (Int64.pred(Int64.shift_left Int64.one (bits_of_width t)))
	 in
	 printf format maskedval
     | Var v ->
	 self#var v
     | UnOp(uop, o) ->
	 (match uop with
	  | NEG -> pp "BVUMINUS("
	  | NOT -> pp "~("
	 );
	 self#ast_exp o;
	 pc ')'
	   (* Eww, the << operator in stp wants a constant int on the right,
	      rather than a bitvector *)
     | BinOp((LSHIFT|RSHIFT|ARSHIFT), e1, Int(i,_)) when i = 0L ->
	 (* STP barfs on 0, so we don't put the shift *)
	 self#ast_exp e1
     | BinOp(LSHIFT, e1, Int(i,_)) ->
	 let  t = infer_ast ~check:false e1 in
	 pp "(("; self#ast_exp e1; pp" << "; pp (Int64.to_string i); pp ")[";
	 pp (string_of_int(bits_of_width t - 1)); pp":0])"
     | BinOp(RSHIFT, e1, Int(i,_)) -> (* Same sort of deal :( *)
	 pc '('; self#ast_exp e1; pp " >> "; pp(Int64.to_string i); pc ')'
     | BinOp(ARSHIFT, e1, Int(i,_)) -> (* Same sort of deal :( *)
	 let t = infer_ast ~check:false e1 in
	 let bits = string_of_int (bits_of_width t) in
	 pp "SX("; self#ast_exp e1; pp " >> "; pp (Int64.to_string i);
	 pp ", "; pp bits; pc ')'
      | BinOp((LSHIFT|RSHIFT|ARSHIFT) as bop, e1, e2) ->
	  let t2 = infer_ast ~check:false e2 in
	  let const n = Int(Int64.of_int n,t2) in
	  let put_one n = self#ast_exp (BinOp(bop, e1, const n)) in
	  let rec put_all n =
	    if n < 64 then (
	      pp " IF ";
	      self#ast_exp e2;
	      pp " = ";
	      self#ast_exp (const n);
	      pp " THEN ";
	      put_one n;
	      pp " ELSE ";
	      put_all (n+1);
	      pp " ENDIF "
	    ) else put_one n
	  in
	  put_all 0;
      | BinOp(bop, e1, e2) ->
	  let t = infer_ast ~check:false e1 in
	  let bits = if is_integer_type t then  bits_of_width t else -1 in
	  let sw = string_of_int bits in
	  let (pre,mid,post) = match bop with
	    | PLUS     -> ("BVPLUS("^sw^", ", ",", ")")
	    | MINUS    -> ("BVSUB("^sw^", ", ",", ")")
	    | TIMES    -> ("BVMULT("^sw^", ", ",", ")")
	    | DIVIDE   -> ("BVDIV("^sw^", ", ",", ")")
	    | SDIVIDE  -> ("SBVDIV("^sw^", ", ",", ")")
	    | MOD      -> ("BVMOD("^sw^", ", ",", ")")
	    | SMOD     -> ("SBVMOD("^sw^", ", ",", ")")
	    | AND      -> ("(", "&", ")")
	    | OR       -> ("(", "|", ")")
	    | XOR      -> ("BVXOR(", ",", ")")
	    | EQ       -> ("IF (", "=", ") THEN 0bin1 ELSE 0bin0 ENDIF")
	    | NEQ      -> ("IF (NOT(", "=", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LT       -> ("IF (BVLT(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LE       -> ("IF (BVLE(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | SLT      -> ("IF (BVSLT(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | SLE      -> ("IF (BVSLE(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LSHIFT 
	    | ARSHIFT
	    | RSHIFT ->
		failwith "shifts should have been handled by a different case"
	  in
	  pp pre;
	  self#ast_exp e1;
	  pp mid;
	  cut();
	  self#ast_exp e2;
	  pp post
      | Cast(ct,t, e1) ->
	  let t1 = infer_ast ~check:false e1 in
	  let (bits, bits1) = (bits_of_width t, bits_of_width t1) in
	  let (pre,post) = match ct with
	    | CAST_SIGNED    -> ("SX(",", "^string_of_int bits^")")
	    | CAST_LOW       -> ("", "["^string_of_int(bits - 1)^":0]")
	    | CAST_HIGH      ->
		("", "["^string_of_int(bits1-1)^":"^string_of_int(bits1-bits)^"]")
	    | CAST_UNSIGNED  ->
		if bits = bits1 then ("","") else
		("(0bin"^String.make (bits-bits1) '0'^" @ ", ")")
	  in
	  pp pre;
	  self#ast_exp e1;
	  pp post
      | Unknown(s,t) ->
	  pp "unknown_"; pi unknown_counter; pp" %"; pp s; force_newline();
	  unknown_counter <- unknown_counter + 1;
      | Lab _ ->
	  failwith "STP: don't know how to handle label names"
      | Let(v, e1, e2) ->
	  pp "(LET ";
	  (* v isn't allowed to shadow anything *)
	  let s = var2s v ^"_"^ string_of_int let_counter in
	  let_counter <- succ let_counter;
	  pp s;
	  pp " =";
	  opn 2; space();
	  self#ast_exp e1;
	  space(); cls();
	  pp "IN"; space();
	  self#extend v s;
	  self#ast_exp e2;
	  self#unextend v;
	  pc ')'
      | Load(arr,idx,endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  self#ast_exp arr;
	  pc '[';
	  self#ast_exp idx;
	  pc ']'
      | Store(arr,idx,vl, endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  pc '(';
	  self#ast_exp arr;
	  pp " WITH [";
	  self#ast_exp idx;
	  pp "] := ";
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

  method assert_ast_exp_with_foralls foralls e =
    opn 0;
    self#declare_new_freevars e;
    force_newline();
    pp "ASSERT(";
    space();
    self#forall foralls;
    pp "0bin1 =";
    force_newline();
    self#ast_exp e;
    force_newline();
    pp ");";
    cls();

  method assert_ast_exp e =
    self#assert_ast_exp_with_foralls [] e


  method close =
    Format.pp_print_newline ft ();

end


class pp_oc fd =
  let ft = Format.formatter_of_out_channel fd in
object
  inherit pp ft as super
  method close =
    super#close;
    close_out fd
end

