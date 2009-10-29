(** C output.


    TODO: Make the interface consistent with that of Pp.
*)
open Type
open Ast
open Printf
open ExtList
module VH = VarHash;;

let find_uninitialized_vars prog =
  let cfg = Vine_cfg.prog_to_cfg prog in
  let live = Vine_dataflow.live_variables ~globals:[] cfg in
    live Vine_cfg.BB_Entry

class pp ?(debug_labels=false) ft =
  let varctx = Hashtbl.create 113 in

  let pp = Format.pp_print_string ft
  and pc = Format.pp_print_char ft
  and space = Format.pp_print_space ft
  and cut = Format.pp_print_cut ft
  and opn = Format.pp_open_hovbox ft
  and opnhv = Format.pp_open_hvbox ft
  and cls = Format.pp_close_box ft
  and pp_int = Format.pp_print_int ft 
  and newline = Format.pp_force_newline ft
  let comma () = pp ","; space() in 
object(self)

  (* the prototypes we assume we have for read and write are:
     uint64_t _mem_read(mem_t m, uint64_t addr, int num_bytes) 
     mem_t _mem_write(mem_t m, uint64_t addr, uint64_t val, int num_bytes)
     you can extend this class if you want something different *)

  val special = "VINE_SPECIAL"
  val mem_load = "VINE_MEM_LOAD"
  val mem_store = "VINE_MEM_STORE"


 (** Pretty print a program using the given formater *)
  method ast_program ?(name="bap_program") sl =
    opn 0;
    pp "#include <bap_helpers.h>\n";
    newline();
    pp "int64_t ";
    pp name;
    pp "() {";
    open_hvbox 2;
    newline();
    opnhv 0; self#format_decls dl; cls();
    newline();
    opnhv 0; self#format_initializations (dl,sl); cls();
    newline();
    opnhv 0;  List.iter self#ast_stmts sl;  cls();
    cls();
    pp "}";
    cls();


    
  method ast_stmt s = 
    opn 2; 
    (match s with
     | Jmp(Lab l, _) ->
	 pp "goto"; space (); pp l; pp ";"
     | Jmp _ ->
	 failwith "Indirect jump not supported yet"
     | CJmp(e1,e2,e3,_) ->
	 pp "if("; self#format_exp e1; pp ")"; space (); pc '{';
	 self#ast_stmt (Jmp(e2,[]));
	 space(); pp "} else {"; space()
	 self#ast_stmt (Jmp(e3,[]));
	 space(); pc '}'
     | Move(v,e,_) ->
	 self#var v;
	 pp " ="; space();
	 self#ast_exp e;
	 pc ';'
     | Special(s,_)->
	 pp special;
	 pp "(\""; pp s; pp "\");"
     | Label(l,_) ->
	 pp l; pc ':'
     | Comment(s,_) ->
	 pp "/*"; pp s; pp "*/"
     | Assert(e,_) ->
	 pp "assert(";
	 self#ast_exp e;
	 pp ");"
     | Halt(e,_) ->
	 pp "exit(";
	 self#ast_exp e;
	 pp ");"
    );
    cls();

  method ast_stmts =
    List.iter self#ast_stmt


  method format_initializations vars =
    let init_var ((_,_,t) as v) =
      pp "VINE_INIT_";
      self#format_typ t;
      pp "(";
      self#format_var v;
      pp ");";
      newline();
    in
      List.iter init_var vars
    


  method ast_exp ?(prec=0) e =
    let fe prec e =
      self#ast_exp ~prec e
    in
    (* prec tells us how much parenthization we need. 0 means it doesn't need
       to be parenthesized. Larger numbers means it has higher precedence.
       Maximum prec before paretheses are added are as follows:
       100 OR
       200 XOR
       300 AND
       400 EQUAL NEQ
       500 LT SLT SLE LE
       600 LSHIFT RSHIFT ARSHIFT
       700 PLUS MINUS
       800 TIMES DIVIDE SDIVIDE MOD
       900 UMINUS NOT
    (* To avoid gcc -Wall warnings, we will artificially require
       operands to | ^ and & to have precedency at least 750 *)
    *)
    let lparen bind = if bind < prec then pp "(" in
    let rparen bind = if bind < prec then pp ")" in
    open_box 0;
    (match e with
       Let _ ->
	 raise (Invalid_argument "Let's not supported. Try calling unlet first.")
     | BinOp(b,e1,e2) ->
	 let op_prec = match b with
	   | BITOR                          -> 100
	   | XOR                            -> 200
	   | BITAND	                  -> 300
	   | EQ | NEQ			  -> 400
	   | LT | SLT | SLE | LE	          -> 500
	   | LSHIFT | RSHIFT | ARSHIFT      -> 600
	   | PLUS | MINUS		          -> 700
	   | TIMES|DIVIDE|SDIVIDE|MOD|SMOD  -> 800
	 in
	 let child_prec = if op_prec <= 300 then 750 else op_prec in
	 lparen op_prec;
	 open_box 0;
	 (* all our binops are left associative *)
	 fe child_prec e1;
	 space();
	 pp (self#binop_to_string b);
	 pp " ";
	 fe (child_prec+1) e2;
	 close_box();
	 rparen op_prec;
	 cut()
     | UnOp(u,e) ->
	 lparen 900;
	 pp(unop_to_string u);
	 fe 900 e;
	 rparen 900
     | Lab l ->
	 pp "name("; pp l; pp ")"
     | Cast(ct,t,e) -> self#format_cast fe ct t e 
     | Unknown(u,t) ->
	 pp "unknown \""; pp u; pp "\""; self#typ t
     | Load(arr,idx,endian, t) ->
	 pp mem_load; pc '('; cut();
	 self#ast_exp arr; comma();
	 self#ast_exp idx; comma();
	 self#ast_exp endian; comma();
	 self#typ t;
	 pc ')'
     | Store(arr,idx,vl, endian, t) ->
	 pp mem_store; pc '('; cut();
	 self#ast_exp arr; comma();
	 self#ast_exp idx; comma();
	 self#ast_exp vl; comma();
	 self#ast_exp endian; comma();
	 self#typ t;
	 pc ')'
	 
    );
    cls()


  method format_var ?(print_type=false) ((_,_,typ) as var) =
    self#format_name var;


  method binop_to_string = function
    | SDIVIDE ->  "/ (int64_t)" 
    | SMOD -> "% (int64_t)"
    | SLT -> "< (int64_t)"
    | SLE -> "<= (int64_t)"
    | RSHIFT -> ">> (uint64_t)"
    | ARSHIFT -> ">> (int64_t)"
    | NEQ -> "!="
    | op -> ( binop_to_string op)


  method typ = function
    | REG_1 -> pp "bool"
    | REG_8 -> pp "uint8_t"
    | REG_16 -> pp "uint16_t"
    | REG_32 -> pp "uint32_t"
    | REG_64 -> pp "uint64_t"
    | TMem(REG_32)-> pp "mem32_t"
    | Array(t2, i) -> pp "mem_norm_t" (* array is normalized memory *) 
    | _ -> raise (Invalid_argument "Unsupported type")


  method format_decl v = 
    self#format_decls [v]

  method format_name ((vid,name,typ) as var) =
    if with_varids then (
      pp name;
      pp "_";
      pp_int vid )
    else
      let strings =
	Stream.from (function
		       | 0 -> Some name
		       | 1 -> Some(name^"_"^string_of_int vid)
		       | 2 -> Some(name^"__"^string_of_int vid)
		       | n -> Some(name^"_"^string_of_int (n-2))
		    )
      in
      let rec pp_next() =
	let s = Stream.next strings in
	  try
	    let var' = Hashtbl.find varctx s in
	      if var == var' then pp s else
		pp_next()
	  with Not_found ->
	    Hashtbl.add varctx s var;
	    pp s
      in
	pp_next()


  method format_decls  dl = 
    let name_sort (_,n1,_) (_,n2,_) = String.compare n1 n2  in
    let idx_sort (i1,_,_) (i2,_,_) = compare i1 i2 in 
    let ctx = Hashtbl.create 19009 in 
      (* map type -> list of variables with that type *)
    let () = List.iter (fun ((_,_,t) as v) -> 
			  if Hashtbl.mem ctx t then
			    Hashtbl.replace ctx t (v::(Hashtbl.find ctx t))
			  else
			    Hashtbl.add ctx t [v]) dl in
    let rec pd vars =  (
      match vars with
	  [] -> () 
	| x::[] -> 	    
	    self#format_var x; 
	    pp ";"
	| x::ys -> 
	    self#format_var  x; pp ","; break 1 0;
	    pd ys
    ) in
    let keys = Hashtbl.fold (fun k _ acc -> k::acc) ctx [] in 
    let ints,others = List.partition (is_integer_type) keys in 
    let ints = List.stable_sort 
      (fun t1 t2 ->
	 match t1,t2 with
	     x,y when is_integer_type t1 && is_integer_type t2 ->
	       compare (bits_of_width t1) (bits_of_width t2)
	   | _ -> -1) ints in
    let keys = List.append ints others in
      List.iter 
	(fun typ ->
	   let vars = Hashtbl.find ctx typ in 
	   let sorted_vars = List.stable_sort  
	     (fun v1 v2 -> 
		if (name_sort v1 v2) <> 0 then 
		  name_sort v1 v2 
		else
		  idx_sort v1 v2) vars in
	     match vars with
		 [] -> ()
	       | _ -> 
		   Format.pp_open_vbox ft 0;
		   Format.pp_open_hbox ft ();
		   self#format_typ  typ;
		   space ();
		   close_box ();
		   Format.pp_open_hovbox ft 0;
		   pd sorted_vars;
		   close_box ();
		   break 0 0;
		   close_box ()
	) keys 


  method format_lval lv = 
      match lv with
	  Temp(v) -> self#format_var v
	| Mem(v,e,t) -> self#mem_read  lv


  method format_value = function
    | Int(REG_1, 1L) -> pp "1"
    | Int(REG_1, 0L) -> pp "0"
    | Int(t,x) -> 
	(* No moding here, since the value should have been modded earlier,
	   and if it wasn't, it's better not to hide it. *)
	pp (if 0L <= x && x < 10L
	    then Int64.to_string x
	    else Printf.sprintf "0x%LxLL" x)
    | Str(x) -> pp "\""; pp x; pp "\""	  


  method format_cast fe  ct t e = 
    let to_signed_type_string t = 
      match t with
	  REG_1 -> "bool"
	| REG_8 -> "int8_t"
	| REG_16 -> "int16_t"
	| REG_32 -> "int32_t"
	| REG_64 -> "int64_t"
	| _ -> raise (Invalid_argument "Invalid integral type")
    in
    let () = open_box 2 in (
	match ct with
	    CAST_LOW 
	  | CAST_UNSIGNED ->
	      (* this avoids warnings in visual studio 2005 *)
	      (* FIXME: but it's wrong! *)
	      if t = REG_1 && ct = CAST_LOW then (
		pp "(((";  self#format_exp e; 
		pp ")&1) == 1)"
	      ) else (
		pp "("; self#format_typ  t; pp ")"; cut(); pp "(";
		self#format_exp  e; pp ")"
	      )
		
	  | CAST_SIGNED when is_integer_type t -> 
	      let tstr = to_signed_type_string t in
		pp "("; pp tstr; pp ")"; cut(); pp "(";
		self#format_exp  e; pp ")"
	  | CAST_HIGH  when is_integer_type t ->
	      (* implements zhenkai's do_cast routine *)
	      let dstlen = bits_of_width t in 
	      let srctyp = Vine_typecheck.infer_type None e in 
	      let srclen = bits_of_width srctyp in 
	      let offset = srclen - dstlen in 
	      let (mask:int64) = Int64.sub 
		(Int64.shift_left 1L dstlen)
		(Int64.one) in 
		pp "("; self#format_typ t; pp ")"; space();
		pp "("; space () ; pp "(";
		pp "("; self#format_exp e; pp ")";
		pp ">>"; pp (string_of_int offset); space ();
		pp ")&"; Format.fprintf ft "0x%LX" mask;
		(* pp (string_of_int mask); *)
		pp ")"
	  | CAST_HIGH 
	  | CAST_SIGNED -> raise (Invalid_argument "Unhandled case")
      ); close_box ()

end


let unlet (dl,sl) = 
  let  unlet_vis =
object(self)
  inherit nop_bap_visitor
    
  val mutable new_decls = []
  val mutable new_stmts = []
  val ctx = VH.create 57

  method visit_rlvalue lv = 
    try
      match lv with
	| Temp(v) -> ChangeTo(Temp(VH.find ctx v))
	| Mem(v,e,t) -> let v' = VH.find ctx v in
	    ChangeTo(Mem(v', (exp_accept self e), t)) 
    with Not_found -> DoChildren
      
  method visit_exp = function
    | Let(l,e1,e2) -> (* visit binding will be called in post *)
	let v = match l with Temp(v) -> v | Mem(v,_,_) -> v in 
	let v' = renewvar v in
	let e1' = exp_accept self e1 in
	let () = VH.add ctx v v' in
	let e2' = exp_accept self e2 in
	  VH.remove ctx v;
	  new_decls <- v'::new_decls;
	  new_stmts <- (match l with
			  | Temp(v) ->
			      Move(Temp v', e1') :: new_stmts
			  | Mem(v,addr,t) ->
			      let addr' = exp_accept self addr in
			      Move(Temp v', Lval(Temp v))
			      :: Move(Mem(v',addr',t), e1')
			      :: new_stmts
		       );
	  ChangeTo e2';
    | _ -> DoChildren

  method visit_stmt s =
    assert(new_decls == [] && new_stmts == []);
    ChangeDoChildrenPost(s, 
		   fun s ->
		     match (new_decls, new_stmts) with
		       | ([],[]) -> s
		       | (dl,sl) -> 
			   new_decls <- [];
			   new_stmts <- [];
			   Block(dl, sl@[s])
		  )
end
  in
  let p' =  prog_accept unlet_vis (dl,sl) in 
    Vine_alphavary.descope_program p'
