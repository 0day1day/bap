(** Bap interface to the parser.

    TODO: Figure out how to best deal with scoping.
*)

let program_from_lexbuf l =
  Grammar.program Lexer.token l

let program_from_file f =
  let ic = open_in f in
  let p = program_from_lexbuf (Lexing.from_channel ic) in
  close_in ic;
  p


let exp_from_lexbuf l =
  Grammar.expr Lexer.token l

let exp_from_string s =
  exp_from_lexbuf (Lexing.from_string s)
