
let sort = 
  let underscore = Str.regexp_string "_" in
  let sort_aux (var1, _) (var2,_) =
    let ss1 = Str.split underscore var1 
    and ss2 = Str.split underscore var2 in
      compare (List.nth ss1 2) (List.nth ss2 2)
  in  
  List.sort sort_aux

let main () =
  let cin = 
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
  let values = Stp_grammar.main Stp_lexer.token lexbuf in
  let sorted = sort values in
    List.iter (fun (_, num) -> Printf.printf "\\x%02Lx" num) sorted

;;

main ()
