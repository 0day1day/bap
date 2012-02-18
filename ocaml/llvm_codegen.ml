(**
   Experimental module for performing LLVM code generation.

   $Id$

   See http://llvm.org/docs/tutorial/OCamlLangImpl3.html for tutorial.
*)

open Ast
open Big_int_convenience
open Llvm
open Llvm_executionengine
open Llvm_scalar_opts
open Type

(** Context for conversion functions *)
type ctx = {
  vars: (Var.t, llvalue) Hashtbl.t;
  mutable allocbb: llbasicblock option
           }

let new_ctx () = { vars=Hashtbl.create 100; allocbb=None }

type varuse = Store | Load

type memimpl = Real | Functional

class codegen =

  let () = ignore (initialize_native_target ()) in
  let context = global_context () in
  let the_module = create_module context "BAP code generator" in
  let builder = builder context in
  let destroy obj = dispose_module the_module in
  (* XXX: Garbage collect execengine *)
  let execengine = ExecutionEngine.create the_module in
  let the_fpm = PassManager.create_function the_module in
  (* Set up the optimizer pipeline.  Start with registering info about
   * how the target lays out data structures. *)
  let () = Llvm_target.TargetData.add (ExecutionEngine.target_data execengine) the_fpm in

  (* Promote allocas to registers. *)
  (* let () = add_memory_to_register_promotion the_fpm in *)

  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  let () = add_instruction_combination the_fpm in

  (* reassociate expressions. *)
  let () = add_reassociation the_fpm in

  (* Eliminate Common SubExpressions. *)
  let () = add_gvn the_fpm in

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  let () = add_cfg_simplification the_fpm in

  let () = ignore (PassManager.initialize the_fpm) in

  (* Find assert function to be called on Assert statements *)
  let assertf = match lookup_function "fake_assert" the_module with
    | None ->
      declare_function "fake_assert" (function_type (void_type context) [| i32_type context |]) the_module
    | Some x -> x
  in

object(self)

  initializer Gc.finalise destroy self

  val ctx = new_ctx ()

  val memimpl = Real

  method convert_type = function
    | Reg n -> integer_type context n
    | _ -> failwith "No idea how to handle memories yet"

  (** Convert a Var to its LLVM pointer *)
  method convert_var (Var.V(_, s, t) as v) =
    try Hashtbl.find ctx.vars v
    with Not_found ->
      let a = self#in_alloc (lazy (build_alloca (self#convert_type t) s builder)) in
      Hashtbl.add ctx.vars v a;
      a

  (** Compile LLVM code to evaluate an Ast.exp. *)
  method convert_exp = function
    | Ite _ as ite -> self#convert_exp (Ast_convenience.rm_ite ite)
    | Extract _ as e -> self#convert_exp (Ast_convenience.rm_extract e)
    | Concat _ as c -> self#convert_exp (Ast_convenience.rm_concat c)
    | Int(i, t) ->
      let lt = self#convert_type t in
      (try const_of_int64 lt (Big_int_Z.int64_of_big_int i) (*signed?*) false
       with Failure _ -> const_int_of_string lt (Big_int_Z.string_of_big_int i) 10)
    | Cast(ct, tto, e) ->
      let tto' = self#convert_type tto in
      let e' = self#convert_exp e in
      (match ct with
      | CAST_UNSIGNED -> build_zext e' tto' "cast_unsigned" builder
      | CAST_SIGNED -> build_sext e' tto' "cast_signed" builder
      | CAST_HIGH ->
        (* HHHLLLLLL

           If want to get the H bits, we need to shift right by W-H *)
        let t = Typecheck.infer_ast e in
        let amount = Typecheck.bits_of_width t -
          Typecheck.bits_of_width tto in
        let () = assert (amount >= 0) in
        let amount = self#convert_exp (Int(biconst amount, t)) in
        let shifted = build_lshr e' amount "high_to_low" builder in
        build_trunc shifted tto' "cast_high" builder
      | CAST_LOW -> build_trunc e' tto' "cast_low" builder)
    | UnOp(uop, e) ->
      let e' = self#convert_exp e in
      (match uop with
      | NEG ->
        let zero = self#convert_exp (Int(bi0, Typecheck.infer_ast e)) in
        (* LLVM has no neg *)
        build_sub zero e' "neg" builder
      | NOT ->
        let negone = self#convert_exp (Int(bim1, Typecheck.infer_ast e)) in
        (* LLVM has no bitwise not *)
        build_xor negone e' "bwnot" builder)
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
    (* How do we handle this properly? *)
    | Ast.Load(Var m, idx, e, t) when memimpl = Real (*&& m = (Var Asmir.x86_mem) *) ->
      let idx' = self#convert_exp idx in
      let idxt = pointer_type (self#convert_type t) in
      let idx'' = build_inttoptr idx' idxt "load_address" builder in
      build_load idx'' "load" builder
    | Ast.Store _ -> failwith "Stores are not proper expressions"
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
    | (Jmp _ | CJmp _ | Label _ | Special _) as s ->
      failwith (Printf.sprintf "convert_straightline_stmt: Non-straightline statement type %s" (Pp.ast_stmt_to_string s))
    | Assert(e, _) ->
      let e' = self#convert_exp e in
      (* Convert to 32-bit to call fake_assert *)
      let c = build_zext e' (i32_type context) "cond" builder in
      ignore(build_call assertf [| c |] "" builder)
    | Halt(e, _) ->
      ignore(build_ret (self#convert_exp e) builder)
    | Move(v, e, _) when Typecheck.is_integer_type (Var.typ v) ->
      let exp = self#convert_exp e in
      let mem = self#convert_var v in
      ignore(build_store exp mem builder)
    (* Simple memory write we understand *)
    (* XXX: How do we make sure this is a write to "the big global
       memory"? *)
    | Move(mv, Ast.Store(Var m,i,v,e,t), _) when Typecheck.is_mem_type (Var.typ mv) && memimpl = Real ->
      let idx' = self#convert_exp i in
      let idxt = pointer_type (self#convert_type t) in
      let idx'' = build_inttoptr idx' idxt "load_address" builder in
      let v' = self#convert_exp v in
      ignore(build_store v' idx'' builder)
    (* XXX: How do we make sure this is a write to "the big global
       memory"? *)
    (* A complicated memory write we need to simplify *)
    | Move(v, _, _) as s when Typecheck.is_mem_type (Var.typ v) ->
      List.iter self#convert_straightline_stmt (Flatten_mem.flatten_stores s)
    | Comment _ -> ()
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
    (* Optimize! *)
    ignore(PassManager.run_function f the_fpm);
    f

  (** Execute lazy value l in the allocbb, and then return to the end
      of the bb the builder was in. *)
  method in_alloc l =
    match ctx.allocbb with
    | None -> failwith "in_alloc: Called with no allocbb set"
    | Some(allocbb) ->
      let save = insertion_block builder in
      (* In the beginning *)
      let () = position_builder (instr_begin allocbb) builder in
      let r = Lazy.force l in
      let () = position_at_end save builder in
      r

  method anon_fun ?(t = void_type context) () =
    let f = declare_function "" (function_type t [||]) the_module in
    let allocbb = append_block context "allocs" f in
    let () = ctx.allocbb <- Some allocbb in
    let bb = append_block context "entry" f in
    (* Add branch from allocbb to entry bb *)
    position_at_end allocbb builder;
    ignore(build_br bb builder);
    position_at_end bb builder;
    f

  method dump =
    dump_module the_module

  method run_fun f =
    ExecutionEngine.run_function f [||] execengine

end
