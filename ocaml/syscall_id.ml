(** Use dataflow analysis to statically identify system call types

    The lattice is a map from each program location to Top | FromSC |
    Syscall | Bottom.  Executing a system call generates
    FromSC. Writing to %eax will convert any addresses at FromSC to
    Syscall.  Writing to %eax for any other state will go to
    Bottom. We also keep track of termination separately.

    @author ejs
*)

open Ast
open Big_int
open Big_int_convenience
open Cfg
module D = Debug.Make(struct let name = "Syscall_id" and default=`NoDebug end)
open D

let syscall_reg = Disasm_i386.eax;;

module AddrMap = Cfg.BM

module DFSPEC = struct
  module G = Cfg.AST.G
  module L = struct
    type littlet = Top | FromSyscall | Syscall of (big_int * Type.typ) | Bottom
    type termt = Term | NoTerm
    type t = termt * littlet AddrMap.t
    let top = NoTerm, AddrMap.empty
    let equal_littlet x y = match x, y with
      | Top, Top -> true
      | FromSyscall, FromSyscall -> true
      | Syscall(bi1, t1), Syscall(bi2, t2) when bi1 ==% bi2 && t1 = t2 -> true
      | Bottom, Bottom -> true
      | _ -> false
    let equal x y = 
      let equal_term = (=) in
      let equal_map x y =
        AddrMap.equal equal_littlet x y
      in
      (equal_term (fst x) (fst y)) && (equal_map (snd x) (snd y))

    let meet_littlet x y = match x, y with
      | FromSyscall, (Syscall _ as sc) -> sc
      | (Syscall _ as sc), FromSyscall -> sc
      | x, y when equal_littlet x y -> x
      | Top, x -> x
      | x, Top -> x
      | _ -> Bottom

    let meet x y =
      let meet_term x y = if x = y then x else Term in
      let meet_map x y =
        AddrMap.fold
          (fun k yv xmap ->
            let xv = try AddrMap.find k xmap
              with Not_found -> Top in
            AddrMap.add k (meet_littlet xv yv) xmap
          ) y x
      in
      meet_term (fst x) (fst y), meet_map (snd x) (snd y)

  end

  let transfer_function g n init =
    let stmts = Cfg.AST.get_stmts g n in
    let process_stmt a s = match s,a with
      | Move(v, Int(i, t), _), (L.Term, m) when v=syscall_reg ->
        (* Meet all addresses with Syscall(i, t) *)
        let newv = L.Syscall(i, t) in
        L.Term, AddrMap.map (fun v -> L.meet_littlet newv v) m
      | Move(v, _, _), (L.Term, m) when v=syscall_reg ->
        (* Meet all addresses with Bottom *)
        let newv = L.Bottom in
        L.Term, AddrMap.map (fun v -> L.meet_littlet newv v) m
      | s, (L.Term, m) when Ast.is_syscall s ->
      (* Meet all existing addresses with bottom, and then meet
         address a with FromSyscall *)
        let newv = L.Bottom in
        let newm = AddrMap.map (fun v -> L.meet_littlet newv v) m in
        let addr = Cfg.AST.G.V.label n in
        L.Term, AddrMap.add addr L.FromSyscall newm
      | _, _ -> a
    in
    (* We don't want to propagate Syscall values across blocks *)
    let init = match init with
      | t, m -> t, (AddrMap.map
                      (fun v -> match v with
                      | L.Syscall _ -> L.Bottom
                      | x -> x)
                      m)
    in
    List.fold_left process_stmt init stmts
  let s0 g =
    (* let check_v v a = *)
    (*   let stmts = Cfg.AST.get_stmts g v in *)
    (*   if List.exists is_syscall stmts then v::a *)
    (*   else a *)
    (* in *)
    (* Cfg.AST.G.fold_vertex check_v g [snd(Cfg_ast.find_error g)] *)
    snd(Cfg_ast.find_exit g)
  let init _ = L.Term, AddrMap.empty
  let dir = GraphDataflow.Backward

end

module DF = GraphDataflow.Make(DFSPEC)

let find_syscalls g =
  let inh, out = DF.worklist_iterate g in
  let process_vertex n a =
    match out n with
    | DFSPEC.L.Term, m ->
      AddrMap.fold (fun k v acc -> match k, v with
      | addr, DFSPEC.L.Syscall(i, t) ->
        dprintf "Found syscall %s! yay" (~% i);
        if debug then (
          let stmts = Cfg.AST.get_stmts g n in
          Debug_snippets.print_ast stmts;
        );
        (n, int_of_big_int i)::acc
      | _ -> acc) m a
    | _ -> a
  in
  Cfg.AST.G.fold_vertex process_vertex g []
