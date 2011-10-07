(** Use dataflow analysis to statically identify system call types

    XXX: The lattice is not correct yet.

    @author ejs
*)

open Ast
open Big_int
open Big_int_convenience
open Cfg
module D = Debug.Make(struct let name = "Syscall_id" and default=`NoDebug end)
open D

let syscall_reg = Disasm_i386.eax;;

module DFSPEC = struct
  module G = Cfg.AST.G
  module L = struct
    type t = Top | Term | FromSyscall | Syscall of (big_int * Type.typ) | Bottom
    let top = Top (* Normal blocks are not system calls *)
    let equal x y = match x, y with
      | Top, Top -> true
      | Term, Term -> true
      | FromSyscall, FromSyscall -> true
      | Syscall(bi1, t1), Syscall(bi2, t2) when bi1 ==% bi2 && t1 = t2 -> true
      | Bottom, Bottom -> true
      | _ -> false
    let meet x y = match x,y with
      | Top, o -> o
      | o, Top -> o
      | Bottom, _ -> Bottom
      | _, Bottom -> Bottom
      | Term, FromSyscall -> FromSyscall
      | FromSyscall, Term -> FromSyscall
      | x, y when equal x y -> x
      | _, _ -> Bottom
  end

  let transfer_function g n init =
    let stmts = Cfg.AST.get_stmts g n in
    let process_stmt a s = match s,a with
      | Move(v, Int(i, t), _), L.FromSyscall when v=syscall_reg -> L.Syscall(i, t)
      | Move(v, _, _), _ when v=syscall_reg -> L.meet a L.Term
      | s, L.Term when Ast.is_syscall s -> L.FromSyscall
      | _, _ -> a
    in
    (* We don't want to propagate Syscall values across blocks *)
    let init = match init with
      | L.Syscall _ -> L.Term
      | _ -> init
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
  let init _ = L.Term
  let dir = GraphDataflow.Backward

end

module DF = GraphDataflow.Make(DFSPEC)

let find_syscalls g =
  let inh, out = DF.worklist_iterate g in
  let process_vertex n a =
    match out n with
    | DFSPEC.L.Syscall(i, t) ->
      let i = int_of_big_int i in
      dprintf "Found syscall %d! yay" i;
      if debug then (
        let stmts = Cfg.AST.get_stmts g n in
        Debug_snippets.print_ast stmts;
      );
      (n, i)::a
    | _ -> a
  in
  Cfg.AST.G.fold_vertex process_vertex g []
