open Ast
module C = Cfg.AST
open Cfg
module D = Debug.Make(struct let name = "Resolve_indirect" and default=`NoDebug end)
open D

let resolve_indjumps ?is_exit asmp cfg =
  let cfg = Hacks.ast_exit_indirect (C.copy cfg) in
  let cfg = Ast_cond_simplify.simplifycond_cfg cfg in

  let rec resolve cfg =

    let _df_in, df_out = Vsa.AlmostVSA.DF.worklist_iterate_widen ~nmeets:50 ~opts:{Vsa.AlmostVSA.DFP.O.initial_mem=Asmir.get_readable_mem_contents_list asmp} cfg in
    C.G.fold_vertex
      (fun v cfg ->
        (try
          Printf.printf "VSA %s" (Cfg_ast.v2s v);
          Vsa.AbsEnv.pp print_string (df_out v);
          print_string "\n\n"
        with Not_found -> ());
        if List.mem (C.G.V.create BB_Indirect) (C.G.succ cfg v) then (
          let stmts = C.get_stmts cfg v in
          match List.rev stmts with
          | Jmp(e, _)::_ ->
            let l = df_out v in
            let vs = Vsa.AlmostVSA.DFP.exp2vs l e in
            dprintf "targets: %s" (Vsa.VS.to_string vs);
            cfg
          | s::_ -> failwith (Printf.sprintf "resolve_indjumps: Expected an indirect jump but found %s" (Pp.ast_stmt_to_string s))
          | [] -> failwith "resolve_indjumps: Empty statement list")
        else cfg
      ) cfg cfg
  in
  resolve cfg
