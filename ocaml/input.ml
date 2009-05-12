
let inputs = ref []



let speclist =
  let addinput i = inputs := i :: !inputs
  (*and set_int64 ref s =
    try ref := Int64.of_string s
    with Failure "int_of_string" ->
      raise(Arg.Bad("Could not parse \""^s^"\" as int64"))*)
  in
  [
    ("-bin",
     Arg.String(fun s-> addinput (`Bin s)),
     "<file> Convert a binary to the IL");
    ("-ir",
     Arg.String(fun s -> addinput (`Ir s)),
     "<file> Read input from an IR file.");
  ]


let get_program () =
  if !inputs = [] then raise(Arg.Bad "No input specified");
  let get_one = function
    | `Ir f ->
	Parser.program_from_file f
    | `Bin f ->
	let p = Asmir.open_program f in
	Asmir.asmprogram_to_vine p
  in
  let rec cat p = function
    | [] -> p
    | arg::args -> cat ((get_one arg)@p) args
  in
  cat [] !inputs

(*  with fixme -> raise(Arg.Bad "Could not open input file")*)
