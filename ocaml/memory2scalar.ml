(*
  This module turns memory references into scalars.  Specifically,
  every memory access in the form M[addr] where addr is the same
  definition/variable will be turned into a scalar mem_addr.  Global
  value renumbering should do this automatically for simple address
  expressions (e.g., R_EBP + c).

  Ed Schwartz
*)

(*
  * Assumes only one endianness is used.
*)

(*module C = Cfg.AST*)
module D = Debug.Make(struct let name = "memtoscalar" and default=`Debug end)
open D
open Ast

(* Convert a program *)
let convert_g p =

  (* 
     Visitor which transforms memory references as it goes 

     I can't think of a nice way of transforming memory references to
     scalars, since memory operations are side-effect free and scalars
     are not.  So, I just do a pattern match for the common cases, e.g.,

     mem = store(mem, addr, blah) -> mem_addr = blah

     or

     load(mem,addr) -> mem_addr

     In particular, this doesn't handle mem = store(store(...), addr,
     blah) or load(store(...),addr).
  *)
  let h = Hashtbl.create 500 in
  let getnewvar mem idx t =
    try Hashtbl.find h (idx) 
    with Not_found ->
      let nv = Var.newvar "memscalar" t
      in Hashtbl.add h (idx) nv;
      nv
  in
  let visitor = object(self)
    inherit Ast_visitor.nop
      
    method visit_stmt = function
      | Move(lv, e, a) as stmt ->	  
	  begin
	    match lv with
	    | Var.V(i, s, Type.TMem(index)) ->
		begin
		  dprintf "A mem move: %s" (Pp.ast_stmt_to_string stmt);
		  match e with
		  | Store(Var(Var.V(i', _, Type.TMem(_)) as srcvar), idx, value, endian, t) ->
		      let nv = getnewvar srcvar idx t in
		      dprintf "%s = %s" (Pp.var_to_string nv) (Pp.ast_exp_to_string value);
		      `ChangeToAndDoChildren (Move(nv, value, []))
		  | Var(Var.V(_, _, Type.TMem(_))) ->
		      (* mem1 = mem2.  Since we assume these are all the same, doesn't matter *)
		      `DoChildren
		  | _ ->
		      failwith "Complex expressions not handled."
		end
	    | _ -> 
		dprintf "A non-mem move: %s" (Pp.ast_stmt_to_string stmt);
		`DoChildren
	  end
	    
      | _ -> `DoChildren
      
    method visit_exp = function
      | Load(Var(arr), idx, endian, t) ->
	  let nv = getnewvar arr idx t in
	  dprintf "Read %s" (Pp.var_to_string nv);
	  `ChangeToAndDoChildren (Var nv)
      | _ -> `DoChildren
  end
  in

  Ast_visitor.prog_accept visitor p
