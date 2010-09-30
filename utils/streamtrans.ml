let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

(* open Bap*)

type ast = Ast.program
(*type astcfg = Cfg.AST.G.t
type ssa = Cfg.SSA.G.t*)

type prog =
  | Ast of ast

type cmd = 
  | TransformAst of (ast -> ast)

let pipeline = ref []


let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

let prints block =
  List.iter
    (fun stmt ->
       Printf.printf "Stmt: %s\n" (Pp.ast_stmt_to_string stmt)
    ) block;
  block

let speclist =
  ("-print", uadd(TransformAst(prints)),
     "Print each statement in the trace.")
  :: Input.stream_speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let pipeline = List.rev !pipeline

let prog =
  try Input.get_stream_program ()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let rec apply_cmd prog = function
  | TransformAst f -> (
      match prog with
      | Ast p -> Ast(f p)
    )
;;

Stream.iter
  (fun block ->
     ignore(List.fold_left apply_cmd (Ast block) pipeline)
  ) prog


