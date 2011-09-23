let usage = "Usage: " ^ Sys.argv.(0) ^ " program"

let speclist = []
let file = ref ""
let anon = (:=) file

;;

Arg.parse speclist anon usage

;;

let pp = new Pp.pp_oc stdout

let p = Asmir.open_program !file
let startaddr = Asmir.get_start_addr p

let get_addr expr =
  match expr with
  | Ast.Int (i, _) -> Some (Big_int.int64_of_big_int i)
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

let rdisasm p startaddr =
  let seen = ref Int64Set.empty in
  let out = ref [] in
  let stack = Stack.create () in
    Stack.push startaddr stack;
    while not (Stack.is_empty stack) do
      let addr = Stack.pop stack in
      try
        let (statement, next) = Asmir.asm_addr_to_bap p addr in
          out := statement :: !out;
          List.iter
            (fun x -> if not (Int64Set.mem x !seen)
              then (Stack.push x stack; seen := Int64Set.add x !seen)
              else ())
            (get_code_addrs statement next)
      (*
       * Ignore invalid addresses.
       * (some programs have a call 0 for some reason)
       *)
      with Asmir.Memory_error -> ()
    done;
    List.concat (List.rev !out)

let ss = rdisasm p startaddr

;;

pp#ast_program ss;
pp#close
