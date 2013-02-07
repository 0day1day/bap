let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

open BatListFull

module VH = Var.VarHash

type ast = Ast.program
(*type astcfg = Cfg.AST.G.t
  type ssa = Cfg.SSA.G.t*)

type prog =
  | Ast of ast

type cmd = 
  | TransformAst of (ast -> ast)
  | AnalysisAst of (ast -> unit)

(** Values for concrete execution *)
let concrete_state = Traces.TraceConcrete.create_state ();;
let mem_hash = Memory2array.create_state ();;
let thread_map = Traces.create_thread_map_state();;
(* HACK to make sure default memory has a map to normalized memory *)
ignore(Memory2array.coerce_rvar_state mem_hash Asmir.x86_mem);;


let pipeline = ref [];;

let final = ref [];;

let add c =
  pipeline := c :: !pipeline

let addfinal c =
  final := c :: !final

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
let concrete block = 
  Traces.concrete_stream mem_hash concrete_state thread_map block false

(* XXX: This should really go somewhere else *)
(** Symbolicly executes a block and builds formulas *)
module MakeStreamSymbolic (TraceSymbolic:Traces.TraceSymbolic with type user_init = Traces.standard_user_init with type output = unit) =
struct

  let last_state = ref None

  let generate_formulas_setup block =
    let block =
      Traces.concrete_stream mem_hash concrete_state thread_map block true
    in
    let block = Traces.remove_specials block in
    let block = Hacks.replace_unknowns block in
    block

  let generate_formulas filename block =
      let block = generate_formulas_setup block in
      let state = match !last_state with
        | Some s -> s
        (* If this is the first block, make a new state *)
        | None ->
          (* Do we need to set dsa_rev_map? *)
          TraceSymbolic.create_state (filename,Smtexec.STP.si)
      in
      last_state :=
        Some (TraceSymbolic.symbolic_run_blocks state block)

  let output_formula () =
    match !last_state with
      | Some s -> TraceSymbolic.output_formula s
      | None -> failwith "Can not output formula for empty state!"
end

module StreamSymbolic =
  MakeStreamSymbolic(Traces.TraceSymbolicStream)

let speclist =
  ("-print", Arg.String(fun f -> add(TransformAst(prints f))),
   "<file> Print each statement in the trace to file.")
  ::("-trace-check",
     Arg.Set Traces.consistency_check,
     "Perform consistency checks"
    )
  ::("-trace-check-all",
     Arg.Set Traces.checkall,
     "Perform extra consistency checks possible when all instructions are logged"
    )
  ::("-trace-formula",
     Arg.String(fun f ->
       add(AnalysisAst(StreamSymbolic.generate_formulas f));
       addfinal(StreamSymbolic.output_formula)
     ),
     "<file> Generate and output a trace formula to <file>.")
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
  | AnalysisAst f -> (
    match prog with
    | Ast p as p' -> f p; p'
  )
;;

(* if (!outfile <> "") then *)
(*   (\** Set for formula generation *\) *)
(*   Traces.dsa_rev_map := Some(rh) *)
(* else ( *)
(*   Traces.checkall := true; *)
(*   Traces.consistency_check := true *)
(* ); *)

Stream.iter
  (fun block ->
    ignore(List.fold_left apply_cmd (Ast block) pipeline)
  ) prog;

List.iter (fun f -> f ()) !final;;
