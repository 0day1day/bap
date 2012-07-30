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


let to_ssagcl ?(usedc=true) ?(usesccvn=true) cfg post =
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
  let gcl = Gcl.of_astcfg cfg in
  (gcl, p);;

let stream_concrete ?(tag = "") mem_hash concrete_state block =
  let block = Memory2array.coerce_prog_state mem_hash block in
  let memv = Memory2array.coerce_rvar_state mem_hash Asmir.x86_mem in
  ignore(Traces.run_block concrete_state memv block);
  []

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

