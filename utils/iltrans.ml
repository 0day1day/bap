let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

open Bap

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
  let pp = new Bap.Pp.pp_oc oc in
  pp#ast_program p;
  pp#close;
  p

let output_ast_bbids f p =
  let oc = open_out f in
  Cfg_pp.AstBBidDot.output_graph oc p;
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

let sccvn p =
  fst(Sccvn.replacer p)
let deadcode p =
  fst(Deadcode.do_dce p)


let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

let speclist =
  ("-pp-ast", Arg.String(fun f -> add(TransformAst(output_ast f))),
   "<file> Pretty print AST to <file>.")
  ::("-pp-ast-bbids", Arg.String(fun f -> add(TransformAstCfg(output_ast_bbids f))),
     "<file> Pretty print AST graph to <file> (in Graphviz format) (no stmts)")
  ::("-pp-ssa", Arg.String(fun f -> add(TransformSsa(output_ssa f))),
     "<file> Pretty print SSA graph to <file> (in Graphviz format)")
  ::("-pp-ssa-bbids", Arg.String(fun f -> add(TransformSsa(output_ssa_bbids f))),
     "<file> Pretty print SSA graph to <file> (in Graphviz format) (no stmts)")
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
  ::("-ssa-simp", uadd(TransformSsa Ssa_simp.simp_cfg),
     "Perform all supported optimizations on SSA")
  ::("-pp-ssa-cdg", Arg.String (fun f -> add(TransformSsa(output_ssa_cdg f))),
     "Output the SSA CDG (bbid's)")
  :: Bap.Input.speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let pipeline = List.rev !pipeline

let prog =
  try Bap.Input.get_program()
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


