(** Use dataflow analysis to statically identify system call types

    The basic idea is to do constant propagation for the system call
    id register (%eax) and then see if a constant reaches a system
    call site.

    XXX: What we really should do is start at the system call sites,
    and go backwards.  The current structure of graphDataflow makes
    this a little difficult, because there is only one start node.
    The reason for this is that a single "int 0x80" could be jumped to
    by several locations that set the system call id reg.
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
    type t = Top | Constant of (big_int * Type.typ) | Syscall of (big_int * Type.typ) | Bottom
    let top = Top
    let equal x y = match x, y with
      | Top, Top -> true
      | Bottom, Bottom -> true
      | Constant(bi1, t1), Constant(bi2, t2) when bi1 ==% bi2 && t1 = t2 -> true
      | Syscall(bi1, t1), Syscall(bi2, t2) when bi1 ==% bi2 && t1 = t2 -> true
      | _ -> false
    let meet x y = match x,y with
      | Top, o -> o
      | o, Top -> o
      | Bottom, _ -> Bottom
      | _, Bottom -> Bottom
      | (Constant _ as x), (Constant _ as y) when equal x y -> x
      | (Syscall _ as x), (Syscall _ as y) when equal x y -> x
      | _, _ -> Bottom
  end

  let transfer_function g n init =
    let stmts = Cfg.AST.get_stmts g n in
    let process_stmt a s = match s,a with
      | Move(v, Int(i, t), _), _ -> L.Constant(i, t)
      | _, L.Syscall _ -> L.Bottom (* After the syscall eax is clobbered *)
      | s, L.Constant(x) when Ast.is_syscall s -> L.Syscall(x)
      | _, _ -> a
    in
    List.fold_left process_stmt init stmts
  let s0 g = snd(Cfg_ast.find_entry g)
  let init _ = L.Bottom (* EAX is undefined at program start *)
  let dir = GraphDataflow.Forward

end

module DF = GraphDataflow.Make(DFSPEC)

let find_syscalls g =
  let _, out = DF.worklist_iterate g in
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
