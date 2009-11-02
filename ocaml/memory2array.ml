(** Convert memory style accesses to array accesses.
    
   This modules converts all TMem references to normalized array references.

    @author Edward J. Schwartz
*)

(* TODO: Handle different endianness.  Use the type of the array expression *)

module D = Debug.Make(struct let name="memory2array" and default=`Debug end)

open D
open Type
open Ast
open Var


(** How big is normalized index? *)
let bitwidth = 8

let getelementtype tmem =
  match tmem with
  | TMem(elementtype) -> elementtype
  | _ -> failwith "This is not an array"

let getwidth regtyp =
  match regtyp with
  | Reg(n) -> n/bitwidth
  | _ -> failwith "Only support register indices!"

let split_load array index eletype endian bytenum =
  let newtype = Reg(bitwidth) in
  let indexplus = BinOp(PLUS, index, Int(Int64.of_int(bytenum), eletype)) in
  let exp = Load(array, indexplus, endian, newtype) in
  let exp = Cast(CAST_UNSIGNED, eletype, exp) in
  (* djb: you also need to mask the value *)
  let exp = BinOp(LSHIFT, exp, Int(Int64.of_int((bytenum) * bitwidth), eletype)) in
  exp
 
let split_loads array index eletype endian =
  assert (endian = exp_false);
  let elesize = getwidth eletype in
  let singlereads = Util.mapn (split_load array index eletype endian) (elesize - 1) in
  let orexp = List.fold_left exp_or (List.hd singlereads) (List.tl singlereads) in
  orexp

let split_write array index eletype endian data bytenum =
  let newtype = Reg(bitwidth) in
  let indexplus = BinOp(PLUS, index, Int(Int64.of_int(bytenum), eletype)) in
  (* djb: you also need to mask the value *)
  let exp = BinOp(RSHIFT, data, Int(Int64.of_int((bytenum) * bitwidth), eletype)) in
  let exp = Cast(CAST_LOW, newtype, exp) in
  let exp = Store(array, indexplus, exp, endian, newtype) in
  exp
      
let split_writes array index eletype endian data =
  assert (endian = exp_false);
  let inftype = Typecheck.infer_ast array in
  let tempvar = newvar "tempmem" inftype in
  let tempastvar = Var(tempvar) in
  let elesize = getwidth eletype in
  let singlewrites = Util.mapn (split_write tempastvar index eletype endian data) (elesize - 2) in
  let singlewrites = singlewrites @ [(split_write array index eletype endian data (elesize - 1))] in
   let letexp = List.fold_left (fun expr new_expr -> Let(tempvar, new_expr, expr)) tempastvar singlewrites in
  letexp


(** This visitor maps each TMem to an array *)
class memory2array_visitor hash
    =
  object (self)
    inherit Ast_visitor.nop
	
    method visit_avar avar =
      match Var.typ(avar) with
      |	TMem(idxt) ->
	  let array =
	    try VarHash.find hash avar
	    with Not_found -> 
	      (* djb: we want the indx type to be the same. The
		 element type changes *)
	      let newarrvar = newvar (Var.name avar) (Array(idxt,Reg(bitwidth))) 
	      in
	      VarHash.add hash avar newarrvar;
	      newarrvar
	  in
          `ChangeToAndDoChildren array (* Do we need to recurse on the avar? *)
      |	_ ->  `DoChildren
	
    method visit_rvar rvar =
      match Var.typ(rvar) with
      |	TMem(idxt) ->
	  let array =
	    try VarHash.find hash rvar
	    with Not_found -> 
	      (* djb: again, i think this is incorrect *)
	      let newarrvar = newvar (Var.name rvar) (Array(idxt,Reg(bitwidth))) 	      
	      in
	      VarHash.add hash rvar newarrvar;
	      newarrvar
	  in
          `ChangeToAndDoChildren array
      |	_ -> `DoChildren
  end

(** This visitor changes each load and store to byte-level operations *)
class memory2array_visitor2 hash
    =
  object (self)
    inherit Ast_visitor.nop

    method visit_stmt stmt =
      `DoChildren
	
    method visit_exp exp =
(*       Printf.printf "Visiting expression %s\n" (Pp.ast_exp_to_string exp); *)
      match exp with
      | Load(arr,idx,endian,t) -> ((* Printf.printf "Load %s\n" (Pp.ast_exp_to_string exp); *)
	  let width = (getwidth t) in
	  match width with
	  | 1 -> (* Printf.printf "Cool\n"; *)
	      `DoChildren
	  | _ -> (* Printf.printf "Need to split\n"; *)
	      let newexpr = split_loads arr idx t endian 
	      in
	      (* Printf.printf "New Load %s\n" (Pp.ast_exp_to_string newexpr); *)
	      (* djb: still need to descend into children *)
	      `ChangeToAndDoChildren newexpr)
      | Store(arr,idx,data,endian,t) -> ((* Printf.printf "Store %s %s %s Reg%d\n" (Pp.ast_exp_to_string arr) (Pp.ast_exp_to_string idx) (Pp.ast_exp_to_string data) (getwidth t); *)
          let width = (getwidth t) in
          match width with
          | 1 -> (* Printf.printf "Cool!\n"; *)
	      `DoChildren
          | _ -> ( (* Printf.printf "Need to split\n"; *)
                  let newexpr = split_writes arr idx t endian data in
		  `ChangeToAndDoChildren newexpr
                    ))
      | _ -> `DoChildren              
      

  end


(** deend your average program. Returns new program where all memory
    broken down to byte-level reads and writes using array variables
    with the same name as the old memory variables.  *)
let coerce_prog prog = 
  let hash = VarHash.create 1000 in
  let visitor = new memory2array_visitor hash
  and visitor2 = new memory2array_visitor2 hash
  in
  let prog = Ast_visitor.prog_accept visitor prog in
  let prog = Ast_visitor.prog_accept visitor2 prog
  in
  prog
  
