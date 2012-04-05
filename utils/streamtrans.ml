let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

open BatListFull

type ast = Ast.program
(*type astcfg = Cfg.AST.G.t
  type ssa = Cfg.SSA.G.t*)

type prog =
  | Ast of ast

type cmd = 
  | TransformAst of (ast -> ast)

let concrete_state = Traces.TraceConcrete.create_state ();;
let mem_hash = Memory2array.create_state ();;
(* HACK to make sure default memory has a map to normalized memory *)
ignore(Memory2array.coerce_rvar_state mem_hash Asmir.x86_mem);;

let pipeline = ref []

let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

(** Prints the block *)
let prints f =
  let oc = open_out f in
  let pp = new Pp.pp_oc oc in
  (fun block ->
    (* List.iter (fun s -> pp#ast_stmt s) block; *)
    pp#ast_program block;
    block)

(** Concretely executes a block *)
let concrete block = Utils_common.stream_concrete mem_hash concrete_state block


let speclist =
  ("-print", Arg.String(fun f -> add(TransformAst(prints f))),
   "<file> Print each statement in the trace to file.")
  ::("-concrete", uadd(TransformAst(concrete)),
     "Concretely execute each block.")
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

Traces.checkall := true;
Traces.consistency_check := true;

Stream.iter
  (fun block ->
    ignore(List.fold_left apply_cmd (Ast block) pipeline)
  ) prog


