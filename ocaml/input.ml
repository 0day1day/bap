open BatListFull
open Big_int_convenience

let init_ro = ref false
let inputs = ref []
and streaminputs = ref None
and streamrate = ref 10000L (* Unless specified grab this many frames at a time *)
and pintrace = ref false

let toint64 s =
  try Int64.of_string s
  with Failure "int_of_string" -> raise(Arg.Bad("invalid int64: "^s))

let tobigint s =
  try Big_int_Z.big_int_of_string s
  with Failure _ -> raise(Arg.Bad("invalid big_int: "^s))

let setint64 r s =  r := toint64 s

let setbigint r s = r := tobigint s

let stream_speclist =
  (* let addinput i = streaminputs := i :: !streaminputs in *)
  [
    ("-rate",
     Arg.String(setint64 streamrate), "<rate> Stream at rate frames");
    ("-tracestream",
     Arg.String(fun s ->
       streaminputs := Some(`Tracestream s)),
     "<file> Read a trace to be processed as a stream.");
  ]

let addinput i = inputs := i :: !inputs

let trace_speclist =
[
    ("-trace",
     Arg.String(fun s ->
       addinput (`Trace s)),
     "<file> Read in a trace and lift it to the IL");
]

let speclist =
  [
    ("-init-ro", Arg.Set (init_ro), "Access rodata.");
    ("-bin",
     Arg.String(fun s -> addinput (`Bin s)),
     "<file> Convert a binary to the IL");
    ("-binrange",
     Arg.Tuple(let f = ref ""
               and s = ref (bi 0) in
               [Arg.Set_string f; Arg.String(setbigint s);
                Arg.String(fun e->addinput(`Binrange(!f, !s, tobigint e)))]),
     "<file> <start> <end> Convert the given range of a binary to the IL");
    ("-binrecurse",
     Arg.String(fun s -> addinput (`Binrecurse s)),
     "<file> Lift binary to the IL using a recursive descent algorithm.");
    ("-binrecurseat",
     Arg.Tuple(let f = ref "" in
               [Arg.Set_string f;
                Arg.String (fun s -> addinput (`Binrecurseat (!f, tobigint s)))]),
     "<file> <start> Lift binary to the IL using a recursive descent algorithm starting at <start>.");
    ("-il",
     Arg.String(fun s -> addinput (`Il s)),
     "<file> Read input from an IL file.");
  ] @ trace_speclist


let get_program () =
  if !inputs = [] then raise(Arg.Bad "No input specified");
  let get_one (oldp,oldscope) = function
    | `Il f ->
      let newp, newscope = Parser.program_from_file ~scope:oldscope f in
      List.append newp oldp, newscope
    | `Bin f ->
      let p = Asmir.open_program f in
      List.append (Asmir.asmprogram_to_bap ~init_ro:!init_ro p) oldp, oldscope
    | `Binrange (f, s, e) ->
      let p = Asmir.open_program f in
      List.append (Asmir.asmprogram_to_bap_range ~init_ro:!init_ro p s e) oldp, oldscope
    | `Binrecurse f ->
      let p = Asmir.open_program f in
      List.append (fst (Asmir_rdisasm.rdisasm p)) oldp, oldscope
    | `Binrecurseat (f, s) ->
      let p = Asmir.open_program f in
      List.append (fst (Asmir_rdisasm.rdisasm_at p [s])) oldp, oldscope
    | `Trace f ->
      List.append (Asmir.serialized_bap_from_trace_file f) oldp, oldscope
  in
  try
    let p,scope = List.fold_left get_one ([], Grammar_private_scope.default_scope ()) (List.rev !inputs) in
    (* Always typecheck input programs. *)
    Printexc.print Typecheck.typecheck_prog p;
    p,scope
  with e ->
    Printf.eprintf "Exception %s occurred while lifting\n" (Printexc.to_string e);
    raise e

(* Determine the architecture mode of the given program *)
let get_program_mode () =
  (* XXX: Assume that all inputs are the same arch (is this correct?) *)
  match !inputs with
  | [] -> raise(Arg.Bad "No input specified");
  | `Il f ::_ -> 
    raise(Arg.Bad "Couldn't get architecture mode (input is IL)")
  | `Bin f :: _ -> 
    Asmir.get_asmprogram_mode (Asmir.open_program f)
  | `Binrange (f, _, _) :: _ -> 
    Asmir.get_asmprogram_mode (Asmir.open_program f)
  | `Binrecurse f :: _ -> 
    Asmir.get_asmprogram_mode (Asmir.open_program f)
  | `Binrecurseat (f, _) :: _ -> 
    Asmir.get_asmprogram_mode (Asmir.open_program f)
  | `Trace f :: _ ->
   (let r = new Trace_container.reader f in
    match r#get_arch, (Int64.to_int r#get_machine) with
    | Arch.Bfd_arch_i386, x when x = Arch.mach_i386_i386 -> Disasm_i386.X86
    | Arch.Bfd_arch_i386, x when x = Arch.mach_x86_64 -> Disasm_i386.X8664
    | _, _ -> raise(Arg.Bad "unsupported architecture")
   )

let get_stream_program () = match !streaminputs with
  | None -> raise(Arg.Bad "No input specified")
  | Some(`Tracestream f) -> 
    Asmir.serialized_bap_stream_from_trace_file !streamrate f


(*  with fixme -> raise(Arg.Bad "Could not open input file")*)
