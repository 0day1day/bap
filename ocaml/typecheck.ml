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

let bits_of_width = function
  | Reg n -> n
  | _ -> invalid_arg "bits_of_width"



let rec infer_ast ?(check=true) = function
  | Var v ->
      (* FIXME: Check context *)
      Var.typ v
  | UnOp(_, e) ->
      infer_ast ~check e
  | BinOp(o, e1,e2) ->
      (* FIXME: checking *)
      (match o with
       | EQ | NEQ | LT | LE | SLT | SLE -> reg_1
       | _ -> infer_ast e1
      )
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
