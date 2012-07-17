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


(** Values for formula generation *)
let h = VH.create 1000;; (* vars to dsa vars *)
let rh = VH.create 10000;;  (* dsa vars to vars *)

let outfile = ref "";;

let traceSymbType = ref Traces.NoSub;;

let pipeline = ref [];;

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
let concrete block = 
  Utils_common.stream_concrete mem_hash concrete_state thread_map block false

(** Symbolicly executes a block and builds formulas *)
module StreamSymbolic (TraceSymbolic:Traces.TraceSymbolicRun) =
struct
  let last_state = ref None;;

  let generate_formulas_setup block = 
    let block = 
      Utils_common.stream_concrete mem_hash concrete_state thread_map block true
    in
    let block = Traces.remove_specials block in
    let block = Hacks.replace_unknowns block in
    block

  let generate_formulas filename block =
    let create_state = TraceSymbolic.SymbolicEval.create_state in
    let init_formula_file = TraceSymbolic.init_formula_file in
    let construct_symbolic = TraceSymbolic.construct_symbolic_run_formula in
    let state = match !last_state with 
      | Some s -> s         
      | None ->
          let block = generate_formulas_setup block in
          create_state (init_formula_file filename)
    in
    last_state := Some (construct_symbolic h rh state block)
      
  let output_formula () = 
    match !last_state with
      | Some s -> TraceSymbolic.Form.ouput_formula s.SymbolicEval.pred
      | None -> failwith "Can not output formula for empty state!"
end

module StreamSymbolicNoSubNoLet = 
  StreamSymbolic(Traces.TraceSymbolicNoSubNoLet)
module StreamSymbolicNoSub = 
  StreamSymbolic(Traces.TraceSymbolicNoSub)
module StreamSymbolicNoSubOpt = 
  StreamSymbolic(Traces.TraceSymbolicNoSubOpt)
module StreamSymbolicSub = 
  StreamSymbolic(Traces.TraceSymbolicSub)

let speclist =
  ("-print", Arg.String(fun f -> add(TransformAst(prints f))),
   "<file> Print each statement in the trace to file.")
  ::("-concrete", uadd(TransformAst(concrete)),
     "Concretely execute each block.")
  ::("-trace-formula",
     Arg.String(fun f -> 
       (outfile := f; 
        add(AnalysisAst(StreamSymbolicNoSub.generate_formulas f)))
     ),
     "<file> Generate and output a trace formula to <file>.  "^
       "Don't use substitution but do use lets.")
  ::("-trace-formula-opt",
     Arg.String(fun f -> 
       (outfile := f; 
        add(AnalysisAst(StreamSymbolicNoSubOpt.generate_formulas f)))),
     "<file> Generate and output a trace formula to <file>.  "^
       "Don't use substitution but do use lets.")
  ::("-trace-formula-no-sub-no-let",
     Arg.String(fun f -> 
       (outfile := f; traceSymbType := Traces.NoSubNoLet;
        add(AnalysisAst(StreamSymbolicNoSubNoLet.generate_formulas f)))),
     "<file> Generate and output a trace formula to <file>.  "^
       "Don't use substitution and do not use lets.")
  ::("-trace-formula-sub",
     Arg.String(fun f -> 
       (outfile := f; traceSymbType := Traces.Substitution;
        add(AnalysisAst(StreamSymbolicSub.generate_formulas f)))),
     "<file> Generate and output a trace formula to <file>.  "^
       "Do use substitution.")
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
      | _ -> failwith "need explicit translation to AST"
  )
;;

(** SWXXX Is there a better way to do this? *)
if (!outfile <> "") then
  (** Set for formula generation *)
  Traces.dsa_rev_map := Some(rh)
else (
  Traces.checkall := true;
  Traces.consistency_check := true
);

Stream.iter
  (fun block ->
    ignore(List.fold_left apply_cmd (Ast block) pipeline)
  ) prog;

(** SWXXX Is there a better way to do this? *)
if (!outfile <> "") then (
  Traces.dsa_rev_map := None;
  print_endline("Outputting formula.");
  match !traceSymbType with
    | Traces.NoSub -> StreamSymbolicNoSub.output_formula ()
    | Traces.NoSubOpt -> StreamSymbolicNoSubOpt.output_formula ()
    | Traces.NoSubNoLet -> StreamSymbolicNoSubNoLet.output_formula ()
    | Traces.Substitution -> StreamSymbolicSub.ouput_formula ()
)

