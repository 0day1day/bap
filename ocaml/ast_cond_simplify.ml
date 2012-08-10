(* See Section 3.4.2, Predicates for Conditional Branch Instructions,
   of Gogul Balakrishnan's dissertation, WYSINWYX: WHAT YOU SEE IS NOT
   WHAT YOU EXECUTE.

   Available at http://pages.cs.wisc.edu/~bgogul/Research/Thesis/bgogul-thesis.pdf
*)

open Ast
module C=Cfg.AST
module D=Debug.Make(struct let name="Ast_cond_simplify" and default=`Debug end)
open D

let simplify_cond g =
  let cp = Copy_prop.copyprop_ast g in
  C.G.iter_vertex (fun v ->
    let stmts = C.get_stmts g v in
    match List.rev stmts with
    | CJmp(e, _, _, _)::tl ->
      let _, copyprop = cp (v, List.length tl) in
      dprintf "e: %s e': %s" (Pp.ast_exp_to_string e) (Pp.ast_exp_to_string (copyprop e))
    | _ -> ()
  ) g;
  g
