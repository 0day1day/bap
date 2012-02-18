(**
   Experimental module for performing LLVM code generation.

   $Id$

   See http://llvm.org/docs/tutorial/OCamlLangImpl3.html for tutorial.
*)

(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Ast
open Llvm
open Type

exception Error of string

let context = global_context ()
let the_module = create_module context "BAP code generator"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let convert_type = function
  | Reg n -> integer_type context n
  | _ -> failwith "No idea how to handle memories yet"

let rec codegen_exp = function
  | Int(i, t) ->
    let lt = convert_type t in
    (try const_of_int64 lt (Big_int_Z.int64_of_big_int i) (*signed?*) false
     with Failure _ -> const_int_of_string lt (Big_int_Z.string_of_big_int i) 10)
  | BinOp(bop, e1, e2) ->
    let le1 = codegen_exp e1 in
    let le2 = codegen_exp e2 in
    let bf = match bop with
      | PLUS -> build_add
      | MINUS -> build_sub
      | TIMES -> build_mul
      | DIVIDE -> build_udiv
      | SDIVIDE -> build_sdiv
      | MOD -> build_urem
      | SMOD -> build_srem
      | LSHIFT -> build_shl
      | RSHIFT -> build_lshr
      | ARSHIFT -> build_ashr
      | AND -> build_and
      | OR -> build_or
      | XOR -> build_xor
      | _ -> failwith "Unsupported binop"
    in
    bf le1 le2 (Pp.binop_to_string bop^"_tmp") builder
  | _ -> failwith "Unsupported exp"

let codegen_exp_helper e =
  let t = Typecheck.infer_ast e in
  let lt = convert_type t in
  let f = declare_function "fakefunc" (function_type lt [||]) the_module in
  let bb = append_block context "entry" f in
  position_at_end bb builder;
  let ret = codegen_exp e in
  ignore(build_ret ret builder);
  (*Llvm_analysis.assert_valid_function f;*)
  f

  (* | Ast.Variable name -> *)
  (*     (try Hashtbl.find named_values name with *)
  (*       | Not_found -> raise (Error "unknown variable name")) *)
  (* | Ast.Binary (op, lhs, rhs) -> *)
  (*     let lhs_val = codegen_expr lhs in *)
  (*     let rhs_val = codegen_expr rhs in *)
  (*     begin *)
  (*       match op with *)
  (*       | '+' -> build_add lhs_val rhs_val "addtmp" builder *)
  (*       | '-' -> build_sub lhs_val rhs_val "subtmp" builder *)
  (*       | '*' -> build_mul lhs_val rhs_val "multmp" builder *)
  (*       | '<' -> *)
  (*           (\* Convert bool 0/1 to double 0.0 or 1.0 *\) *)
  (*           let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in *)
  (*           build_uitofp i double_type "booltmp" builder *)
  (*       | _ -> raise (Error "invalid binary operator") *)
  (*     end *)
