(*pp camlp4o pa_macro.cmo *)

(** Functions that are used by utilities and tests *)

open Ast
module D=Debug.Make(struct let name = "Utils_common" and default=`NoDebug end)
open D
open Type
open BatListFull

(* For solving predicates *)
let rename_astexp f =
  let vis = object
    inherit Ast_visitor.nop
    method visit_rvar v =
      try ChangeTo(f v)
      with Not_found -> DoChildren
  end in
  Ast_visitor.exp_accept vis;;


let optimize_cfg ?(usedc=true) ?(usesccvn=true) cfg post =
  let cfg = Hacks.remove_cycles cfg in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  let cfg = Coalesce.coalesce_ast cfg in
  let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let p = rename_astexp tossa post in
  let cfg =
    let vars = Formulap.freevars p in
    Ssa_simp.simp_cfg ~liveout:vars ~usedc ~usesccvn cfg
  in
  let cfg = Cfg_ssa.to_astcfg cfg in
  (cfg, p);;

let stream_concrete mem_hash concrete_state thread_map block return =
  let block = Memory2array.coerce_prog_state mem_hash block in
  let memv = Memory2array.coerce_rvar_state mem_hash Asmir.x86_mem in
  let block = Traces.explicit_thread_stmts block thread_map in
  if return then
    Traces.run_block ~transformf:Traces.trace_transform_stmt concrete_state memv thread_map block
  else (
    ignore(Traces.run_block concrete_state memv thread_map block);
    []
  )

let get_functions ?unroll ?names p =
  let ranges = Asmir.get_function_ranges p in
  let do_function (n,s,e) =
    let inc = match names with
      | Some l -> List.mem n l
      | None -> true
    in
    try
      if inc then (
        let ir = Asmir.asmprogram_to_bap_range p s e in
        let ir = Hacks.ret_to_jmp ir in
        let cfg = Cfg_ast.of_prog ir in
        let cfg = Prune_unreachable.prune_unreachable_ast cfg in
        let cfg = match unroll with
          | Some n ->
            let cfg = Unroll.unroll_loops ~count:n cfg in
            Hacks.remove_cycles cfg
          | None -> cfg
        in
        let cfg = Prune_unreachable.prune_unreachable_ast cfg in
        let ir = Cfg_ast.to_prog cfg in
        Some (n,ir,cfg))
      else None
    with ex ->
      Printf.eprintf "Warning: problem with %s (0x%Lx-0x%Lx): %s\n" n s e (Printexc.to_string ex);
      None
  in
  BatList.filter_map do_function ranges

let jitexecute inits p =
IFDEF WITH_LLVM THEN
  let cfg = Cfg_ast.of_prog p in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  let codegen = new Llvm_codegen.codegen Llvm_codegen.FuncMulti in
  let jit = codegen#convert_cfg cfg in
  let r = codegen#eval_fun ~ctx:inits jit in
  Printf.printf "Result: %s\n" (Pp.ast_exp_to_string r)
ELSE
  failwith "LLVM not enabled"
END;;


