(** Bap interface to the parser.

    TODO: Figure out how to best deal with scoping.
*)

let program_from_lexbuf l =
  let p = Grammar.program Lexer.token l in
  Parsing.clear_parser();
  p

let program_from_file f =
  let ic = open_in f in
  let lb = Lexing.from_channel ic in
  lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
  let p = program_from_lexbuf lb in
  close_in ic;
  p


let exp_from_lexbuf l =
  let e = Grammar.expr Lexer.token l in
  Parsing.clear_parser();
  e

let exp_from_string s =
  exp_from_lexbuf (Lexing.from_string s)
