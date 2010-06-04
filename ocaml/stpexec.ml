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

let select_timeout = 0.1 (* seconds *)

let alarm_handler i =
  raise Alarm_signal_internal

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
(*   let ic = in_channel_of_descr stdoutread in *)
(*   let ec = in_channel_of_descr stderrread in *)
  let cmdname, args = split_cmdstr cmd in
  let pid = create_process cmdname args stdinread stdoutwrite stderrwrite in
  (try
     let obuf = Buffer.create 16 in
     let ebuf = Buffer.create 16 in
     let fdlist = [stdoutread; stderrread] in
     List.iter (fun fd -> set_nonblock fd) fdlist;
     let wait = ref true in
     let estatus = ref None in
     let buf = String.create 1 in
     while !wait do    
       
       (* Read any new data from stdout/stderr *)
       let rd () =
	 let rl,_,_ = select fdlist [] fdlist select_timeout in
	 List.iter
	   (fun fd ->
	      let buffer = if fd=stdoutread then obuf else ebuf in
	      try
		while true do
		  (match read fd buf 0 1 with
		   | 1 ->
		       Buffer.add_string buffer buf
		   | _ ->
		       failwith "Assertion error");
		done
	      with Unix_error _ -> ()
	   ) rl;
(* 	 dprintf "Hmm: %d %d" (List.length rl) (List.length el); *)
       in

       (* Do a read *)
       rd ();
       
       (* Check if the process is dead yet *)
       let pid',estatus' = waitpid [WNOHANG] pid in
       if pid' = pid then begin
	 wait := false;
	 estatus := Some(estatus')
       end;
       
       (* Do another read, in case the process died *)
       rd();
       
     done;
     close stdoutread;
     close stdoutwrite;
     close stderrread;
     close stderrwrite;
     close stdinread;
     close stdinwrite;
     (Buffer.contents obuf), (Buffer.contents ebuf), Util.option_unwrap !estatus
   with Alarm_signal_internal ->
     raise (Alarm_signal pid))


(* let syscall' cmd = *)
(*   let () = Sys.set_signal Sys.sigalrm (Sys.Signal_handle alarm_handler) in *)
(*   let ic, oc, ec = Unix.open_process_full cmd (Unix.environment ()) in *)
(*   let obuf = Buffer.create 16 in *)
(*   let ebuf = Buffer.create 16 in *)
(*   (try *)
(*      while true do Buffer.add_channel obuf ic 1 done *)
(*    with End_of_file -> ()); *)
(*   (try *)
(*      while true do Buffer.add_channel ebuf ec 1 done *)
(*    with End_of_file -> ()); *)
(*   let estatus = Unix.close_process_full (ic, oc, ec) in *)
(*   (Buffer.contents obuf), (Buffer.contents ebuf), estatus *)
 
let result_to_string result =
  match result with
  | Valid -> "Valid"
  | Invalid -> "Invalid"
  | StpError -> "StpError"
  | Timeout -> "Timeout"

let stp = "/home/ed/f09/stp-ver-0.1-11-18-2008/bin/stp"
let sh = "/bin/bash"

let runstp ?(timeout=60) file =
  ignore(alarm timeout);

  try
    (
      let cmdstr = Printf.sprintf "cvc3 +unknown-check-model %s" file in
      let stpout,stperr,pstatus = syscall cmdstr
      in

(*       dprintf "ok: %s %s" stpout stperr; *)
      
      (* Turn the alarm off *)
      ignore(alarm 0);
      
      let fail = match pstatus with
	| WEXITED(c) ->
	    
	    if c > 0 then (
(* 	      dprintf "FAIL code: %d" c; *)
	      true 
	    )
	    else false
	      
	| _ -> true
      in
      let fail = fail or ExtString.String.exists stperr "Fatal" in
      let isinvalid = ExtString.String.exists stpout "Invalid." in
      let isvalid = ExtString.String.exists stpout "Valid." in
      
(*       dprintf "fail: %b %b %b" fail isinvalid isvalid; *)
      
      if isvalid then
	Valid
      else if isinvalid then
	Invalid
      else if fail then (
	dprintf "STP output: %s\nSTP error: %s" stpout stperr;  
	StpError
      )
      else
	failwith "Something weird happened."
    )
  with
    Alarm_signal(pid) -> 
      (* Another HUGE hack *)
(*       ignore(Unix.system "killall -9 stp; killall -9 cvc3");       *)
      kill pid 9;
      Timeout ;;

(* let runstp ?(timeout=60) file = *)
(*   let pid = fork () in *)
(*   match pid with *)
(*   | 0 -> ((\* child *\)       *)
(* (\*       ignore(alarm timeout); *\) *)
(*       (\* Yes, this is the biggest hack ever. *\) *)
(*       let cmdstr = "perl -e 'alarm shift @ARGV; exec @ARGV' " ^ string_of_int timeout ^ " sh -c 'stp -sv " ^ file ^ " | grep Valid'" in *)
(*       dprintf "Cmdstr: %s" cmdstr; *)
(*       ignore(Unix.execv sh [| "sh"; "-c"; cmdstr |]); *)
      
(*       exit 2 *)
(*     ) *)

(*   | cpid -> ((\* parent *\) *)
(*       let (pid,status) = (\* waitpid [] cpid *\) wait () in *)
(*       dprintf "Pid %d exited" pid; *)

(*       (\* Another HUGE hack *\) *)
(*       ignore(Unix.system "killall -9 stp"); *)

(*       (match status with *)
(*       | WEXITED(code) -> dprintf "exit: %d" code *)
(*       | WSIGNALED(signum) -> dprintf "signal: %d" signum *)
(*       | _ -> dprintf "Wtf"); *)


(*       match status with *)
(*       | WEXITED(code) when code = 0 -> Valid *)
(*       | WEXITED(code) when code = 1 -> Invalid *)
(*       | WSIGNALED(signum) when signum = Sys.sigalrm -> Timeout *)
(*       | _ -> failwith "Unexpected termination" *)

(*     ) *)

let compute_wp_boring cfg post =
  let gcl = Gcl.of_astcfg cfg in
    (Wp.wp gcl post, [])

(** Write formula for AST CFG out to random filename and return the filename *)
let writeformula ?(exists=[]) ?(foralls=[]) ?(remove=true) p  =
    let name, oc = Filename.open_temp_file ("formula" ^ (string_of_int (getpid ())))  ".stp" in
    at_exit (fun () -> 
	       if remove then
		 try 
		   Unix.unlink name
		 with _ -> ());
    dprintf "Using temporary file %s" name;  
    let p = Prune_unreachable.prune_unreachable_ast p in
    let post = Ast.exp_true in
    let (wp, _foralls) = compute_wp_boring p post in
    let m2a = new Memory2array.memory2array_visitor () in
    let wp = Ast_visitor.exp_accept m2a wp in
    let foralls = List.map (Ast_visitor.rvar_accept m2a) foralls in
    let pp = new Stp.pp_oc oc in
    
    pp#valid_ast_exp ~exists:exists ~foralls:foralls wp;
    pp#flush ();
(*     output_string oc "QUERY(FALSE); COUNTEREXAMPLE;\n"; *)
    pp#close;
    name

