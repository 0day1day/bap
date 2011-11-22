(** Recursive disassembly module

    @author Ricky
*)

open Ast
open BatListFull
module D = Debug.Make(struct let name = "Asmir_rdisasm" and default=`NoDebug end)
open D
open Type

let get_addr expr =
  match expr with
  | Ast.Int (i, _) -> Some (Big_int_Z.int64_of_big_int i)
  | _ -> None

let collect_some f l x =
    match f x with
    | Some i -> i :: l
    | None -> l

(*
 * Given a BAP IR for an instruction and the address of the next instruction,
 * returns a list of addresses that we should disassemble.
 *)
let get_code_addrs stmts next =
  let rec
    get_code_addrs' stmts l =
      match stmts with
      (* We must explore the following instructions next.

         To see why, consider what happens with a conditional jump:

         addr 0xd2928 @asm "je     0x00000000000d29b4"
         label pc_0xd2928
         cjmp R_ZF:bool, 0xd29b4:u32, "nocjmp0"
         label nocjmp0

         If the condition is false, control transfers to the next
         instruction.  This is expected to be the next instruction in
         memory. If we recursed to the target first, then we would
         build the IL with the new target there, and thus implicitly
         transfer control to that address even when the condition is
         false!

         As a result of this, next should be at the front of the list,
         and we can use a Queue. *)
      | [] -> next :: l
      | (Ast.Jmp (e, attrs)) :: _ ->
        let addrs = collect_some get_addr l e in
          (* Assume that control returns to the next instruction a call. *)
          if List.mem (Type.StrAttr "call") attrs
          then next :: addrs
          else addrs
      | (Ast.CJmp (_, e1, e2, _)) :: rest ->
          let addrs = List.fold_left (collect_some get_addr) [] [e1; e2]
          in
            (*
             * Only continue looking for addresses operands wasn't an address.
             * This usually means that the operand was a label at the next
             * statement.
             *)
            if List.length addrs == 2
            then addrs
            else get_code_addrs' rest (addrs @ l)
      | _ :: rest -> get_code_addrs' rest l
  in
  get_code_addrs' stmts []

module Int64Set = Set.Make( 
  struct
    type t = Int64.t
    let compare = Int64.compare
  end)

type callback = addr -> addr -> stmt list -> bool
let default _ _ _ = true

(** Recursively disassemble [p] beginning at [startaddr]. If [f] is
    defined and [f addr stmts] returns false, raises
    {!Asmir.Disassembly_error}. *)
let rdisasm_at ?(f=default) p startaddr =
  let seen = ref Int64Set.empty in
  let out = ref [] in
  let outasm = ref "" in
  let stack = Queue.create () in
  let numstmts = ref 0 in
  Queue.push startaddr stack;
  while not (Queue.is_empty stack) do
    let addr = Queue.pop stack in
    try
      let (statements, next) = Asmir.asm_addr_to_bap p addr in
      let asm = Asmir.get_asm_instr_string p addr in
      out := statements :: !out;
      outasm := !outasm ^ "; " ^ asm;
      numstmts := !numstmts + (List.length statements);
      if not (f addr next statements) then raise Asmir.Disassembly_error;
      List.iter
        (fun x -> if not (Int64Set.mem x !seen)
          then (Queue.push x stack; seen := Int64Set.add x !seen)
          else ())
        (get_code_addrs statements next)
    (*
     * Ignore invalid addresses.
     * (some programs have a call 0 for some reason)
     *)
    with Asmir.Memory_error -> ()
  done;
  List.concat (List.rev !out), !outasm

(** Recursively disassemble [p] beginning at the program's defined start
    address.  [f] behaves the same as in {!rdisasm_at}. *)
let rdisasm ?(f=default) p =
  rdisasm_at ~f p (Asmir.get_start_addr p)

let max_callback n =
  let ctr = ref 0 in
  (fun _ _ _ ->
    incr ctr;
    if !ctr > n then false else true)
