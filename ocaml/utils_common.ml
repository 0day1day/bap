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
      try `ChangeTo(f v)
      with Not_found -> `DoChildren
  end in
  Ast_visitor.exp_accept vis;;


let to_ssagcl ?(usedc=true) ?(usesccvn=true) cfg post =
  let cfg = Hacks.remove_cycles cfg in
  let cfg = Coalesce.AST_Coalesce.coalesce cfg in
  let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let p = rename_astexp tossa post in
  let cfg =
    let vars = Formulap.freevars p in
    Ssa_simp.simp_cfg ~liveout:vars ~usedc ~usesccvn cfg
  in
  let cfg = Cfg_ssa.to_astcfg cfg in
  let gcl = Gcl.of_astcfg cfg in
  (gcl, p);;


(* For type checking.  Temporary home. *)
let typecheck p =
  let v = object(self)
    inherit Ast_visitor.nop
    method visit_exp e = ignore(Typecheck.infer_ast ~check:true e); `DoChildren
  end 
  in
  List.iter
    (fun s ->
      try ignore(Ast_visitor.stmt_accept v s)
      with Typecheck.TypeError _ as e ->
        (* Having the statement usually helps *)
        wprintf "Problem statement: %s" (Pp.ast_stmt_to_string s);
        raise e
    )
    p;
  p

let stream_concrete ?(tag = "") mem_hash concrete_state block =
  let block = Memory2array.coerce_prog_state mem_hash block in
  let memv = Var.VarHash.find mem_hash Asmir.x86_mem in
  ignore(Traces.run_block concrete_state memv block);
  []
