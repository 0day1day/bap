open Asmir_disasm
module CS = Cfg.SSA
module D = Debug.Make(struct let name = "Switch_condition" and default = `NoDebug end)
open D
open Ssa

let add_switch_conditions {origssa; optssa; vsa_in} =
  CS.G.fold_vertex (fun v g ->
    (match CS.get_stmts g v with
    | Jmp(e, _)::_ when Ssa.lab_of_exp e = None ->
      dprintf "indirect jump %s" (Pp.ssa_exp_to_string e)
    | _ -> ());
    g
  ) origssa origssa
