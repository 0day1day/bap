(** Convert memory style accesses to array accesses.

   This modules converts all TMem references to normalized array references.

    @author Edward J. Schwartz
*)

(* TODO: Handle different endianness.  Use the type of the array expression *)

(* XXX: What should we do when there is a 1 bit access? *)

module D = Debug.Make(struct let name="memory2array" and default=`Debug end)
open D

open Ast
open Ast_convenience
open BatListFull
open Big_int_Z
open Grammar_scope
open Type
open Util
open Var

let numelems t mt =
  let bits = Typecheck.bits_of_width t in
  let elebits = Typecheck.bits_of_width mt in
  if (bits mod elebits) <> 0 then failwith "Expected all memory accesses to be multiples of the element type";
  bits / elebits

let split_load array index indextype valuetype accesstype endian bytenum =
  let indexplus = BinOp(PLUS, index, Int(big_int_of_int bytenum, indextype)) in
  let exp = Load(array, indexplus, endian, valuetype) in
  let exp = Cast(CAST_UNSIGNED, accesstype, exp) in
  let exp = exp_shl exp (Int(big_int_of_int (bytenum * (Typecheck.bits_of_width valuetype)), accesstype)) in
  exp

let split_load_list array index indextype valuetype loadtype endian =
  (* XXX: remove me *)
  assert (endian === exp_false);
  let nele = numelems loadtype valuetype in
  let mvar = Var_temp.nt "loadnorm" (Array(indextype, valuetype)) in
  (Util.mapn (split_load (Var mvar) index indextype valuetype loadtype endian) (nele - 1), mvar)

let split_loads array index loadtype endian =
  let t = Typecheck.infer_ast array in
  let indextype = Typecheck.index_type_of t in
  let valuetype = Typecheck.value_type_of t in
  let (singlereads, mvar) = split_load_list array index indextype valuetype loadtype endian in
  let orexp = List.fold_left exp_or (List.hd singlereads) (List.tl singlereads) in
  Let(mvar, array, orexp)

let split_write array index indextype valuetype accesstype endian data bytenum =
  let indexplus = BinOp(PLUS, index, Int(big_int_of_int bytenum, indextype)) in
  let exp = exp_shr data (Int(big_int_of_int (bytenum * Typecheck.bits_of_width valuetype), accesstype)) in
  let exp = Cast(CAST_LOW, valuetype, exp) in
  let exp = Store(array, indexplus, exp, endian, valuetype) in
  exp

let split_write_list array index indextype valuetype storetype endian data =
  assert (endian === exp_false);
  let tempmemvar = Var_temp.nt "tempmem" (Array(indextype, valuetype)) in
  let tempvalvar = Var_temp.nt "tempval" storetype in
  let nelems = numelems storetype valuetype in
  let singlewrites = Util.mapn (split_write (Var tempmemvar) index indextype valuetype storetype endian (Var tempvalvar)) (nelems - 2) in
  (singlewrites @ [(split_write array index indextype valuetype storetype endian (Var tempvalvar) (nelems - 1))], tempmemvar, tempvalvar)

let split_writes array index storetype endian data =
  let t = Typecheck.infer_ast array in
  let indextype = Typecheck.index_type_of t in
  let valuetype = Typecheck.value_type_of t in
  let (singlewrites, tempmemvar, tempvalvar) = split_write_list array index indextype valuetype storetype endian data in
  let letexp = List.fold_left (fun expr new_expr -> Let(tempmemvar, new_expr, expr)) (Var tempmemvar) singlewrites in
  Let(tempvalvar, data, letexp)


class memory2array_visitor ?scope hash =

  let get_array_var (Var.V(_, _, t) as avar) =
    match t with
    | TMem (idxt, Reg bitwidth) ->
      let array =
        try VarHash.find hash avar
        with Not_found ->
          (* We haven't mapped variable v before. If a scope of
             variables was provided, let's see if v_array is in
             scope. *)
          let new_name = (Var.name avar)^"_array" in
          let new_type = Array (idxt, Reg bitwidth) in
          let make_new_var () = newvar new_name new_type in
          let newarrvar = match scope with
            | Some(scope) ->
              (try Scope.get_lval_if_defined scope new_name (Some new_type)
               with Not_found -> make_new_var ())
            | None ->
              make_new_var ()
          in

          (* Map in the m2a hash *)
          VarHash.add hash avar newarrvar;

          (* Update the scope if defined *)
          (match scope with
          | Some(scope) ->
            (* First, ensure that if new_name is already in Scope, that
               it maps to newarrvar.  If not, the memory2array hash and
               Scope are out of sync, which indicates someone did
               something bad. *)
            (try assert (Var.equal (Scope.get_lval_if_defined scope new_name (Some new_type)) newarrvar)
             with Not_found -> (* If not in Scope, it's okay *) ());

            (* Now, update the Scope *)
            ignore(Scope.add_var scope new_name newarrvar);

          | None -> ());

          newarrvar
      in
      array
    | _ -> failwith "get_array_var expects a TMem variable only"
  in

object (self)
  inherit Ast_visitor.nop

  method visit_avar avar =
    match Var.typ avar with
    | TMem _ ->
      ChangeToAndDoChildren (get_array_var avar)
    |   _ ->
      DoChildren

  method visit_rvar = self#visit_avar

  method visit_exp exp =
    (* Printf.printf "Visiting expression %s\n" (Pp.ast_exp_to_string exp); *)
    match exp with
    | Load(arr,idx,endian,t) -> (
      let width = numelems t (Typecheck.value_type_of (Typecheck.infer_ast arr)) in
      match width with
      | 1 ->
        DoChildren
      | _ ->
        let arr = Ast_visitor.exp_accept self arr in
        let newexpr = split_loads arr idx t endian
        in
        ChangeToAndDoChildren newexpr)
    | Store(arr,idx,data,endian,t) -> (
      let width = numelems t (Typecheck.value_type_of (Typecheck.infer_ast arr)) in
      match width with
      | 1 ->
        DoChildren
      | _ ->
        let arr = Ast_visitor.exp_accept self arr in
        let newexpr = split_writes arr idx t endian data in
        ChangeToAndDoChildren newexpr)
    | _ -> DoChildren
  end

type state = Ast.var VarHash.t

let create_state () = VarHash.create 1000

let coerce_prog prog =
  let hash = create_state () in
  let visitor = new memory2array_visitor hash in
  Ast_visitor.prog_accept visitor prog

let coerce_prog_state ?scope hash prog =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.prog_accept visitor prog

let coerce_exp exp =
  let hash = create_state () in
  let visitor = new memory2array_visitor hash in
  Ast_visitor.exp_accept visitor exp

let coerce_exp_state ?scope hash exp =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.exp_accept visitor exp

let coerce_rvar_state ?scope hash v =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.rvar_accept visitor v
