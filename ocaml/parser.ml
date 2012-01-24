(** Bap interface to the parser. *)

open Grammar_private_scope
open Grammar_scope

(* Note: Even though scope is imperative, we need to return it since the
   user might not have specified the input scope, but may be interested
   in the output scope. *)

let program_from_lexbuf ?(scope=default_scope ()) l =
  set_scope scope;
  let p = Grammar.program Lexer.token l in
  Parsing.clear_parser();
  p, get_scope()

let program_from_file ?(scope=default_scope ()) f =
  let ic = open_in f in
  let lb = Lexing.from_channel ic in
  lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
  let p = program_from_lexbuf lb ~scope in
  close_in ic;
  p


let exp_from_lexbuf ?(scope=default_scope ()) l =
  set_scope scope;
  let e = Grammar.expr Lexer.token l in
  Parsing.clear_parser();
  e, get_scope()

let exp_from_string ?(scope=default_scope ()) s =
  exp_from_lexbuf (Lexing.from_string s) ~scope

let exp_from_file ?(scope=default_scope ()) f =
  let ic = open_in f in
  let lb = Lexing.from_channel ic in
  lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
  let e = exp_from_lexbuf lb ~scope in
  close_in ic;
  e
