let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

(* open Bap*)

type ast = Ast.program
type astcfg = Cfg.AST.G.t
type ssa = Cfg.SSA.G.t

type prog =
  | Ast of ast
  | AstCfg of astcfg
  | Ssa of ssa

type cmd = 
  | TransformAst of (ast -> ast)
  | TransformAstCfg of (astcfg -> astcfg)
  | TransformSsa of (ssa -> ssa)
  | ToCfg
  | ToAst
  | ToSsa
 (* add more *)

let pipeline = ref []

let output_ast f p =
  let oc = open_out f in
  let pp = new Pp.pp_oc oc in
  pp#ast_program p;
  pp#close;
  p

let output_ast_bbids f p =
  let oc = open_out f in
  Cfg_pp.AstBBidDot.output_graph oc p;
  close_out oc;
  p

let output_ast_cdg f p =
  let oc = open_out f in 
  let cdg = Depgraphs.CDG_AST.compute_cdg p in 
    Cfg_pp.AstBBidDot.output_graph oc cdg;
    close_out oc;
    p
 
let output_ast_pdg f p = 
  let oc = open_out f in 
  let pdg = Depgraphs.PDG_AST.compute_pdg p in 
    Cfg_pp.AstStmtsDot.output_graph oc pdg;
    close_out oc;
    p 

let output_ssa f p =
  let oc = open_out f in
  Cfg_pp.SsaStmtsDot.output_graph oc p;
  close_out oc;
  p
let output_ssa_bbids f p =
  let oc = open_out f in
  Cfg_pp.SsaBBidDot.output_graph oc p;
  close_out oc;
  p

let output_ssa_cdg f p =
  let oc = open_out f in 
  let cdg = Depgraphs.CDG_SSA.compute_cdg p in 
    Cfg_pp.SsaBBidDot.output_graph oc cdg;
    close_out oc;
    p

let output_ssa_ddg f p = 
  let oc = open_out f in 
  let ddg = Depgraphs.DDG_SSA.compute_ddg p in 
    Cfg_pp.SsaStmtsDot.output_graph oc ddg;
    close_out oc;
    p

let sccvn p =
  fst(Sccvn.replacer p)
let deadcode p =
  fst(Deadcode.do_dce p)
let jumpelim p =
  Deadcode.cfg_jumpelim p
let ast_coalesce = Coalesce.AST_Coalesce.coalesce
let ssa_coalesce = Coalesce.SSA_Coalesce.coalesce

(* Chop code added *)
let chop srcbb srcn trgbb trgn p = 
  Depgraphs.CHOP_AST.chop p srcbb srcn trgbb trgn

(* Evaluation code added *)
let eval_ast p = 
  Search.dfs_ast_program p ;
  p 

let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

let speclist =
  ("-pp-ast", Arg.String(fun f -> add(TransformAst(output_ast f))),
   "<file> Pretty print AST to <file>.")
  ::("-pp-ast-bbids", Arg.String(fun f -> add(TransformAstCfg(output_ast_bbids f))),
     "<file> Pretty print AST graph to <file> (in Graphviz format) (no stmts)")
  ::("-pp-ast-cdg", Arg.String (fun f -> add(TransformAstCfg(output_ast_cdg f))),
     "Output the AST CDG (bbid's)")
  ::("-ast-eval", uadd(TransformAst eval_ast),
     "Evaluate an AST and print variable values on exit")
  ::("-pp-ast-pdg", Arg.String (fun f -> add(TransformAstCfg(output_ast_pdg f))),
     "Output the AST DDG (bbid's)")
  ::("-pp-ast-chop", 
      Arg.Tuple 
        (let srcbb = ref 0 and srcn = ref 0 
         and trgbb = ref 0 and trgn = ref 0 in
         [Arg.Set_int srcbb ; Arg.Set_int srcn ;
          Arg.Set_int trgbb ; Arg.Set_int trgn ; 
               uadd(TransformAstCfg(chop !srcbb !srcn !trgbb !trgn)) ]),
     "<src-bb> <src-num> <trg-bb> <trg-num> Calculate the chop of an AST")
  ::("-pp-ssa", Arg.String(fun f -> add(TransformSsa(output_ssa f))),
     "<file> Pretty print SSA graph to <file> (in Graphviz format)")
  ::("-pp-ssa-bbids", Arg.String(fun f -> add(TransformSsa(output_ssa_bbids f))),
     "<file> Pretty print SSA graph to <file> (in Graphviz format) (no stmts)")
  ::("-pp-ssa-cdg", Arg.String (fun f -> add(TransformSsa(output_ssa_cdg f))),
     "Output the SSA CDG (bbid's)")
  ::("-pp-ssa-ddg", Arg.String (fun f -> add(TransformSsa(output_ssa_ddg f))),
     "Output the SSA DDG (bbid's)")
  ::("-to-cfg", uadd(ToCfg),
     "Convert to an AST CFG.")
  ::("-to-ast", uadd(ToAst),
     "Convert to the AST.")
  ::("-to-ssa", uadd(ToSsa),
     "Convert to SSA.")
  ::("-sccvn", uadd(TransformSsa sccvn),
     "Apply Strongly Connected Component based Value Numbering")
  ::("-deadcode", uadd(TransformSsa deadcode),
     "Perform dead code ellimination.")
  ::("-ast-coalesce", uadd(TransformAstCfg ast_coalesce),
     "Perform coalescing on the AST.")
  ::("-ssa-coalesce", uadd(TransformSsa ssa_coalesce),
     "Perform coalescing on the SSA.")
  ::("-jumpelim", uadd(TransformSsa jumpelim),
     "Control flow optimization.")
  ::("-ssa-simp", uadd(TransformSsa Ssa_simp.simp_cfg),
     "Perform all supported optimizations on SSA")
  ::("-ssa-to-single-stmt", 
     uadd(TransformSsa Depgraphs.DDG_SSA.stmtlist_to_single_stmt),
     "Create new graph where every node has at most 1 SSA statement"
    )
  ::("-trace-cut", Arg.Int(fun i -> add(TransformAst(Util.take i))),
     "<n>  Get the first <n> instructions of the trace")
  ::("-trace-concrete", 
     uadd(TransformAst Traces.concrete),
     "Execute the trace concretely and obtain a straightline trace"
    )
  ::("-trace-concolic", 
     uadd(TransformAst Traces.concolic),
     "Execute the trace symbolically and generate the formula"
    )
  :: ("-normalize-mem",
      uadd(TransformAst Memory2array.coerce_prog),
      "Normalize memory accesses as array accesses"
     )
  :: Input.speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let pipeline = List.rev !pipeline

let prog =
  try Input.get_program()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let rec apply_cmd prog = function
  | TransformAst f -> (
      match prog with
      | Ast p -> Ast(f p)
      | _ -> failwith "need explicit translation to AST"
    )
  | TransformAstCfg f -> (
      match prog with
      | AstCfg p -> AstCfg(f p)
      | _ -> failwith "need explicit translation to AST CFG"
    )
  | TransformSsa f -> (
      match prog with
      | Ssa p -> Ssa(f p)
      | _ -> failwith "need explicit translation to SSA"
    )
  | ToCfg -> (
      match prog with
      | Ast p -> AstCfg(Cfg_ast.of_prog p)
      | Ssa p -> AstCfg(Cfg_ssa.to_astcfg p)
      | AstCfg _ as p -> prerr_endline "Warning: null transformation"; p
    )
  | ToAst -> (
      match prog with
      | AstCfg p -> Ast(Cfg_ast.to_prog p)
      | p -> apply_cmd (apply_cmd p ToCfg) ToAst
    )
  | ToSsa -> (
      match prog with
      | AstCfg p -> Ssa(Cfg_ssa.of_astcfg p)
      | p -> apply_cmd (apply_cmd p ToCfg) ToSsa
    )
;;

List.fold_left apply_cmd (Ast prog) pipeline


