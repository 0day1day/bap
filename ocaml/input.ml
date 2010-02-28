

let inputs = ref []
and pintrace = ref false

let speclist =
  let addinput i = inputs := i :: !inputs in
  let toint64 s =
    try Int64.of_string s
    with Failure "int_of_string" -> raise(Arg.Bad("invalid int64: "^s))
  in
  let setint64 r s =  r := toint64 s in
  [
    ("-bin",
     Arg.String(fun s-> addinput (`Bin s)),
     "<file> Convert a binary to the IL");
    ("-binrange",
     Arg.Tuple(let f = ref ""
               and s = ref 0L in
               [Arg.Set_string f; Arg.String(setint64 s);
                Arg.String(fun e->addinput(`Binrange(!f, !s, toint64 e)))]),
     "<file> <start> <end> Convert the given range of a binary to the IL");
    ("-trace",
     Arg.String(fun s-> addinput (`Trace s)),
     "<file> Read in a trace and lift it to the IL");
    ("-il",
     Arg.String(fun s -> addinput (`Il s)),
     "<file> Read input from an IL file.");
    ("-ir", (* to be removed in next versions *)
     Arg.String(fun s -> addinput (`Il s)),
     "<file> Read input from an IL file. (deprecated)");
    ("-pin", (* enable pin trace *)
     Arg.Set pintrace,
     "Enable pin trace");
  ]



let get_program () =
  if !inputs = [] then raise(Arg.Bad "No input specified");
  let get_one = function
    | `Il f ->
	Parser.program_from_file f
    | `Bin f ->
	let p = Asmir.open_program f in
	Asmir.asmprogram_to_bap p
    | `Binrange (f, s, e) ->
	let p = Asmir.open_program f in
	Asmir.asmprogram_to_bap_range p s e
    | `Trace f ->
    Asmir.bap_from_trace_file ~pin:!pintrace f
  in
  let rec cat p = function
    | [] -> p
    | arg::args -> cat ((get_one arg)@p) args
  in
  cat [] !inputs

(*  with fixme -> raise(Arg.Bad "Could not open input file")*)
