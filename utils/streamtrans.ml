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

let offset = ref 0;;
let concrete_state = Traces.TraceConcrete.create_state ();;
let mem_hash = Var.VarHash.create 1000;;
(* HACK to make sure default memory has a map to normalized memory *)
ignore(Memory2array.coerce_rvar_state mem_hash Asmir.x86_mem);;

let pipeline = ref []

let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

(** Prints the block *)
let prints block =
  print_endline "new block";
  List.iter
    (fun stmt ->
      print_endline ("Stmt: "^ (Pp.ast_stmt_to_string stmt))
    ) block;
  print_endline "end block";
  block

(** Concretely executes a block *)
let concrete block =
  let block = Memory2array.coerce_prog_state mem_hash block in
  let no_specials = Traces.remove_specials block in
  let memv = Var.VarHash.find mem_hash Asmir.x86_mem in
  (* prints block; *)
  Util.print_obj_info "concrete_state" concrete_state;
    (* Ignore output of run_block and return [] to limit memory consumption *)
    (* ignore(Traces.run_blocks ~concrete_state blocks memv); *)
    (* ignore(Traces.run_block ~next_label concrete_state memv no_specials); *)
    (* The following is based on run_blocks.  It's probably to reimplement 
       run_blocks then reproduce the code here. *)
  ignore(Traces.run_block concrete_state memv block);
  []

let speclist =
  ("-print", uadd(TransformAst(prints)),
   "Print each statement in the trace.")
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


