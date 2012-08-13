(* See Section 3.4.2, Predicates for Conditional Branch Instructions,
   of Gogul Balakrishnan's dissertation, WYSINWYX: WHAT YOU SEE IS NOT
   WHAT YOU EXECUTE.

   Available at http://pages.cs.wisc.edu/~bgogul/Research/Thesis/bgogul-thesis.pdf
*)

open Ast
open Big_int_convenience
module C=Cfg.AST
module D=Debug.Make(struct let name="Ast_cond_simplify" and default=`Debug end)
open D
open Type

let rec reverse_visit f e =
  let g = reverse_visit f in
  let e = match e with
    | Load(e1,e2,e3,t1) -> Load(g e1, g e2, g e3, t1)
    | Store(e1,e2,e3,e4,t1) -> Store(g e1, g e2, g e3, g e4, t1)
    | Ite(e1,e2,e3) -> Ite(g e1, g e2, g e3)
    | Extract(h,l,e) -> Extract(h, l, g e)
    | Concat(e1,e2) -> Concat(g e1, g e2)
    | BinOp(bt,e1,e2) -> BinOp(bt, g e1, g e2)
    | UnOp(ut,e) -> UnOp(ut, g e)
    | (Var _ | Lab _ | Int _ | Unknown _) as e -> e
    | Cast(ct,t,e) -> Cast(ct, t, g e)
    | Let(v,e,e') -> Let(v, g e, g e')
  in
  let e' = f e in
  (* If we changed e, use f on it again *)
  if e' <> e then g e' else e'

let simplify_flat = function
  | BinOp(EQ,
          Int(i0, _),
          BinOp(MINUS, e1, e2)) when i0 = bi0 ->
    (* e - e2 = 0 -> e = e2 *)
    BinOp(EQ, e1, e2)
  | BinOp(OR,
          BinOp(LT, e1, e2),
          BinOp(EQ, e1', e2')) when e1 = e1' && e2 = e2' ->
    (* a < b || a == b -> a <= b *)
    BinOp(LE, e1, e2)
  | e -> e

let simplify_exp = reverse_visit simplify_flat

let simplifycond_cfg g =
  let cp = Copy_prop.copyprop_ast g in
  C.G.iter_vertex (fun v ->
    let stmts = C.get_stmts g v in
    match List.rev stmts with
    | CJmp(e, _, _, _)::tl ->
      let _, copyprop = cp (v, List.length tl) in
      dprintf "e: %s e': %s" (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (simplify_exp (copyprop e)))
    | _ -> ()
  ) g;
  g
