(**
   Snippets of debugging code
*)

(** AST debugging visitor *)

let print_ast p =
  let v = object(self)
    inherit Ast_visitor.nop
      (* Add each variable to definedvars *)
    method visit_stmt stmt =
      Printf.printf "Stmt: %s\n" (Pp.ast_stmt_to_string stmt);
      `SkipChildren
  end 
  in
  ignore(Ast_visitor.cfg_accept v p)

