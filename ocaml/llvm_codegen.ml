(**
   Experimental module for performing LLVM code generation.

   $Id$

   See http://llvm.org/docs/tutorial/OCamlLangImpl3.html for tutorial.
*)

(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Ast
open Big_int_convenience
open Llvm
open Llvm_executionengine
open Type

(** Context for conversion functions *)
type ctx = { vars: (Var.t, llvalue) Hashtbl.t }

let new_ctx () = { vars=Hashtbl.create 100 }

type varuse = Store | Load

class codegen =

  let () = ignore (initialize_native_target ()) in
  let context = global_context () in
  let the_module = create_module context "BAP code generator" in
  let builder = builder context in
  let destroy obj = dispose_module the_module in
  (* XXX: Garbage collect execengine *)
  let execengine = ExecutionEngine.create the_module in
  let assertf = match lookup_function "fake_assert" the_module with
    | None ->
      declare_function "fake_assert" (function_type (void_type context) [| i32_type context |]) the_module
    | Some x -> x
  in

object(self)

  initializer Gc.finalise destroy self

  val ctx = new_ctx ()

  method convert_type = function
    | Reg n -> integer_type context n
    | _ -> failwith "No idea how to handle memories yet"

  (** Convert a Var to its LLVM pointer *)
  method convert_var (Var.V(_, s, t) as v) =
    try Hashtbl.find ctx.vars v
    with Not_found ->
      (* XXX: Put this alloca somewhere appropriate *)
      let a = build_alloca (self#convert_type t) s builder in
      Hashtbl.add ctx.vars v a;
      a

  (** Compile LLVM code to evaluate an Ast.exp. *)
  method convert_exp = function
    | Int(i, t) ->
      let lt = self#convert_type t in
      (try const_of_int64 lt (Big_int_Z.int64_of_big_int i) (*signed?*) false
       with Failure _ -> const_int_of_string lt (Big_int_Z.string_of_big_int i) 10)
    | BinOp(bop, e1, e2) ->
      let le1 = self#convert_exp e1 in
      let le2 = self#convert_exp e2 in
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
        | EQ -> build_icmp (Llvm.Icmp.Eq)
        | NEQ -> build_icmp (Llvm.Icmp.Ne)
        | LT -> build_icmp (Llvm.Icmp.Ult)
        | LE -> build_icmp (Llvm.Icmp.Ule)
        | SLT -> build_icmp (Llvm.Icmp.Slt)
        | SLE -> build_icmp (Llvm.Icmp.Sle)
      in
      bf le1 le2 (Pp.binop_to_string bop^"_tmp") builder
    | Var(v) ->
      let mem = self#convert_var v in
      build_load mem (Var.name v) builder
    | _ -> failwith "Unsupported exp"

  (* (\** Create an anonymous function to compute e *\) *)
  (* method convert_exp_helper e = *)
  (*   let t = Typecheck.infer_ast e in *)
  (*   let lt = self#convert_type t in *)
  (*   let f = declare_function "" (function_type lt [||]) the_module in *)
  (*   let bb = append_block context "entry" f in *)
  (*   position_at_end bb builder; *)
  (*   let ret = self#convert_exp e in *)
  (*   ignore(build_ret ret builder); *)
  (*   Llvm_analysis.assert_valid_function f; *)
  (*   f *)

  (** Convert a single straight-line statement *)
  method convert_straightline_stmt = function
    | Jmp _ | CJmp _ | Label _ | Special _ ->
      failwith "convert_straightline_stmt: Non-straightline statement type"
    | Assert(e, _) ->
      let e' = self#convert_exp e in
      (* Convert to 32-bit to call fake_assert *)
      let c = build_zext e' (i32_type context) "cond" builder in
      ignore(build_call assertf [| c |] "" builder)
    | Halt(e, _) ->
      ignore(build_ret (self#convert_exp e) builder)
    | Move(v, e, _) ->
      let exp = self#convert_exp e in
      let mem = self#convert_var v in
      ignore(build_store exp mem builder)
    | _ -> failwith "convert_straightline_stmt: Unimplemented"

  (** Convert straight-line code (multiple statements) *)
  method convert_straightline p =
    List.iter self#convert_straightline_stmt p;

  method convert_straightline_f p =
    let rt = match List.rev p with
    | Halt(e, _)::_ ->
      self#convert_type (Typecheck.infer_ast e)
    | _ -> failwith "Must end in halt"
    in
    let f = self#anon_fun ~t:rt () in
    self#convert_straightline p;
    Llvm_analysis.assert_valid_function f;
    f

  method anon_fun ?(t = void_type context) () =
    let f = declare_function "" (function_type t [||]) the_module in
    let bb = append_block context "entry" f in
    position_at_end bb builder;
    f

  method dump =
    dump_module the_module

  method run_fun f =
    ExecutionEngine.run_function f [||] execengine

end
