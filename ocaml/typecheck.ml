(** Type checking for BAP.


    This is incomplete, as it is somewhat of a quick hack to get type
    inference working (which is needed for STP translation).
*)

open Type
open Ast

exception TypeError of string

let terror s = raise(TypeError s)

(* returns true if t1 is a subtype of t2 *)
let subt t1 t2 =
  t1 = t2

let check_subt t1 t2 f =
  if not(subt t1 t2) then
    terror (Printf.sprintf f (Pp.typ_to_string t1) (Pp.typ_to_string t2))

let is_integer_type = function
  | Reg _ -> true
  | TMem _ | Array _ -> false

let is_mem_type t = not (is_integer_type t)

let bits_of_width = function
  | Reg n -> n
  | _ -> invalid_arg "bits_of_width"

let bytes_of_width t =
  let b = bits_of_width t in
  assert ((b mod 8) = 0);
  b / 8

let rec infer_ast ?(check=true) = function
  | Var v ->
      (* FIXME: Check context *)
      Var.typ v
  | UnOp(_, e) ->
      if check then 
	(let t = infer_ast ~check e in
	check_reg t);
      infer_ast ~check:false e;
  | BinOp(o,e1,e2) ->
      if check then (
	let t1 = infer_ast ~check e1
	and t2 = infer_ast ~check e2 in
	check_same t1 t2; 
	check_reg t1);
      (match o with
       | EQ | NEQ | LT | LE | SLT | SLE -> reg_1
       | _ -> infer_ast ~check:false e1
      )
  | Ite(b,e1,e2) ->
      if check then 
	(let t1 = infer_ast ~check e1
	and t2 = infer_ast ~check e2 in
	check_same t1 t2);
      infer_ast ~check:false e1 
  | Lab s ->
      (* FIXME: no type for labels yet *)
      reg_64
  | Int(_,t)
  | Unknown(_,t) ->
      t
  | Cast(ct,t,e) ->
      (* FIXME: check *)
      t
  | Let(v,e1,e2) ->
      (* FIXME: check *)
      infer_ast e2
  | Load(arr,idx,endian, t) ->
      if check then check_idx arr idx endian t;
      t
  | Store(arr,idx,vl, endian, t) ->
      if check then (
	check_idx arr idx endian t;
	let tv = infer_ast vl in
	check_subt tv t "Can't store value with type %s as a %s";
      );
      infer_ast ~check:false arr

and check_same t1 t2 =
  if t1 <> t2 then
    terror "Similar types expected"

and check_reg t =
  if not (is_integer_type t) then
    terror "Expected integer type"

and check_idx arr idx endian t =
  let ta = infer_ast arr
  and ti = infer_ast idx
  and te = infer_ast endian in
  if te <> reg_1 then terror "Endian must be a boolean";
  if not(is_integer_type ti) then terror "Index must be a register type";
  match ta with
  | Array(i,e) ->
      check_subt ti i "Index type not suitable for indexing into this array. Was %s, needed %s.";
      check_subt t e "Can't get a %s from array of %s";
  | TMem _ -> ();
  | _ -> terror "Indexing only allowed from array or mem."
