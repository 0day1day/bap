(**
   Module for executing STP inside of BAP

   This is a pretty big hack, and is not portable in any shape or way.
*)

exception Alarm_signal_internal;;
exception Alarm_signal of int;;

module D = Debug.Make(struct let name = "Stpexec" and default=`Debug end)
open D

open Unix

type result = Valid | Invalid | StpError | Timeout


module type SOLVER_INFO =
sig
  val timeout : int (** Default timeout in seconds *)
  val solvername : string (** Solver name *)
  val cmdstr : string -> string (** Given a filename, produce a command string to invoke solver *)
  val parse_result : string -> string -> Unix.process_status -> result (** Given output, decide the result *)
end

module type SOLVER =
sig
  val solve_formula_file : ?timeout:int -> string -> result (** Solve a formula in a file *)
  val solve_formula_exp : ?timeout:int -> ?exists:(Ast.var list) -> ?foralls:(Ast.var list) -> Ast.exp -> result (** Solve a formula in an exp *)
end

let select_timeout = 0.1 (* seconds *)

let alarm_handler i =
  raise Alarm_signal_internal

let compute_wp_boring cfg post =
  let gcl = Gcl.of_astcfg cfg in
    (Wp.wp gcl post, [])

(** Write given formula out to random filename and return the filename *)
let write_formula ?(exists=[]) ?(foralls=[]) ?(remove=true) f  =
    let name, oc = Filename.open_temp_file ("formula" ^ (string_of_int (getpid ())))  ".stp" in
    at_exit (fun () -> 
	       if remove then
		 try 
		   Unix.unlink name
		 with _ -> ());
    dprintf "Using temporary file %s" name;  
    let pp = new Stp.pp_oc oc in
    pp#valid_ast_exp ~exists:exists ~foralls:foralls f;
    pp#flush ();
(*     output_string oc "QUERY(FALSE); COUNTEREXAMPLE;\n"; *)
    pp#close;
    name

(** Write formula for AST CFG out to random filename and return the filename *)
let create_formula ?(exists=[]) ?(foralls=[]) ?remove p  =
    let p = Prune_unreachable.prune_unreachable_ast p in
    let post = Ast.exp_true in
    let (wp, _foralls) = compute_wp_boring p post in
    let m2a = new Memory2array.memory2array_visitor () in
    let wp = Ast_visitor.exp_accept m2a wp in
    let foralls = List.map (Ast_visitor.rvar_accept m2a) foralls in
    (* FIXME: same for exists? *)
    write_formula ~exists ~foralls ?remove wp


(* let query_formula ?timeout ?exists ?foralls f = *)
(*   let filename = write_formula ?exists ?foralls f in *)
(*   runstp ?timeout filename *)

(** Convert space delimited command string (command arg1 arg2 ... ) to a command and argument array *)
let split_cmdstr cmdstr =
  let slist = ExtString.String.nsplit cmdstr " " in
  let cmd = List.hd slist in
  let args = Array.of_list slist in
  cmd, args

let syscall cmd =
  let () = Sys.set_signal Sys.sigalrm (Sys.Signal_handle alarm_handler) in
  let (stdoutread,stdoutwrite) = pipe () in
  let (stderrread,stderrwrite) = pipe () in
  let (stdinread,stdinwrite) = pipe () in
  let cmdname, args = split_cmdstr cmd in
  let pid = create_process cmdname args stdinread stdoutwrite stderrwrite in
  close stdoutwrite;
  close stderrwrite;
  close stdinread;
  close stdinwrite;
  (try
     let obuf = Buffer.create 16 in
     let ebuf = Buffer.create 16 in
     let fdlist = [stdoutread; stderrread] in
     List.iter set_nonblock fdlist;
     let wait = ref true in
     let estatus = ref None in
     let buf = String.create 1 in
     while !wait do    
       
       (* Read any new data from stdout/stderr *)
       let rd timeout  =
	 let rl,_,_ = select fdlist [] fdlist timeout in
	 List.iter
	   (fun fd ->
	      let buffer = if fd=stdoutread then obuf else ebuf in
	      try
		while true do
		  (match read fd buf 0 1 with
		   | 1 ->
		       Buffer.add_string buffer buf
		   | 0 -> raise Exit
		   | _ ->
		       failwith "Assertion error");
		done
	      with Exit | Unix_error(EWOULDBLOCK,_,_) | Unix_error(EAGAIN,_,_) -> ()
	   ) rl;
(* 	 dprintf "Hmm: %d %d" (List.length rl) (List.length el); *)
       in

       (* Do a read *)
       rd select_timeout;
       
       (* Check if the process is dead yet *)
       let pid',estatus' = waitpid [WNOHANG] pid in
       if pid' = pid then begin
	 wait := false;
	 estatus := Some(estatus')
       end;
       
       (* Do another read, in case the process died *)
       rd 0.0;
       
     done;
     close stdoutread;
     close stderrread;
     (Buffer.contents obuf), (Buffer.contents ebuf), Util.option_unwrap !estatus
   with Alarm_signal_internal ->
     close stdoutread;
     close stderrread;
     raise (Alarm_signal pid) )
 
let result_to_string result =
  match result with
  | Valid -> "Valid"
  | Invalid -> "Invalid"
  | StpError -> "StpError"
  | Timeout -> "Timeout"

module Make = functor (S: SOLVER_INFO) ->
  struct
    let solve_formula_file ?(timeout=S.timeout) file =
      ignore(alarm timeout);
      let cmdline = S.cmdstr file in

      try
	let sout,serr,pstatus = syscall cmdline in
	
	(* Turn the alarm off *)
	ignore(alarm 0);

	S.parse_result sout serr pstatus

      with Alarm_signal(pid) ->
	kill pid 9;
	Timeout

    let solve_formula_exp ?timeout ?exists ?foralls f =
      let filename = write_formula ?exists ?foralls f in
      solve_formula_file ?timeout filename

  end;;

module STP_INFO =
struct
  let timeout = 60
  let solvername = "stp"
  let cmdstr f = "stp -t " ^ f
  let parse_result stdout stderr pstatus =
    let failstat = match pstatus with
      | WEXITED(c) -> c > 0
      | _ -> true
    in
    let fail = failstat || ExtString.String.exists stderr "Fatal" in
    let isinvalid = ExtString.String.exists stdout "Invalid." in
    let isvalid = ExtString.String.exists stdout "Valid." in
    
    (*       dprintf "fail: %b %b %b" fail isinvalid isvalid; *)
    
    if isvalid then
      Valid
    else if isinvalid then
      Invalid
    else if fail then (
      dprintf "STP output: %s\nSTP error: %s" stdout stderr;  
      StpError
    )
    else
      failwith "Something weird happened."
end

module STP = Make(STP_INFO)

module CVC3_INFO =
struct
  let timeout = 60
  let solvername = "cvc3"
  let cmdstr f = "cvc3 " ^ f
  let parse_result stdout stderr pstatus =
    let failstat = match pstatus with
      | WEXITED(c) -> c > 0
      | _ -> true
    in
    let fail = failstat || ExtString.String.exists stderr "Fatal" in
    let isinvalid = ExtString.String.exists stdout "Invalid." in
    let isvalid = ExtString.String.exists stdout "Valid." in
    
    (*       dprintf "fail: %b %b %b" fail isinvalid isvalid; *)
    
    if isvalid then
      Valid
    else if isinvalid then
      Invalid
    else if fail then (
      dprintf "CVC output: %s\nCVC error: %s" stdout stderr;  
      StpError
    )
    else
      failwith "Something weird happened."
end

module CVC3 = Make(CVC3_INFO)

let runstp = STP.solve_formula_file
let query_formula = STP.solve_formula_exp
