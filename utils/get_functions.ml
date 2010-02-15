
let usage = "Usage: "^Sys.argv.(0)^" <elf file> (<output prefix> | -r) [<function names>]\n\
             Disassemble functions from a binary."

let rangeonly = ref false
let speclist =
  ("-r", Arg.Set rangeonly,
   "Print ranges rather than disassembling functions.")
    :: Input.speclist

let file = ref ""
let prefix = ref ""
let names = ref []
let n = ref 0
let anon x =
  (match !n with
   | 0 -> file := x
   | 1 -> prefix := x
   | _ -> names := x :: !names
  );
  incr n
;;

Arg.parse speclist anon usage;
names := List.rev !names;
if !rangeonly && !prefix <> "" then
  names := !prefix :: !names;
if !file = "" then (
  Arg.usage speclist usage;
  exit 1
);
if !prefix = "" then rangeonly := true

let p = Asmir.open_program !file
let ranges = Asmir.get_function_ranges p

let doit = match !rangeonly with
  | true ->
      (fun (n,s,e) -> Printf.printf "%s\t0x%Lx 0x%Lx\n" n s e)
  | false ->
      (fun (n,s,e) ->
	 let ir = Asmir.asmprogram_to_bap_range p s e in
	 let oc = open_out (!prefix ^ n ^ ".il") in
	 let pp = new Pp.pp_oc oc in
	 pp#ast_program ir;
	 pp#close;
      )
;;
(* Fixme: only for given functions *)
List.iter doit ranges
