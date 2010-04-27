(**
   Module for executing STP inside of BAP

   This is a pretty big hack, and is not portable in any shape or way.
*)

exception Alarm_signal;;

module D = Debug.Make(struct let name = "Stpexec" and default=`Debug end)
open D

open Unix

type result = Valid | Invalid | StpError | Timeout

let alarm_handler i =
  raise Alarm_signal

let syscall cmd =
  let () = Sys.set_signal Sys.sigalrm (Sys.Signal_handle alarm_handler) in
  let ic, oc, ec = Unix.open_process_full cmd (Unix.environment ()) in
  let obuf = Buffer.create 16 in
  let ebuf = Buffer.create 16 in
  (try
     while true do Buffer.add_channel obuf ic 1 done
   with End_of_file -> ());
  (try
     while true do Buffer.add_channel ebuf ec 1 done
   with End_of_file -> ());
  let estatus = Unix.close_process_full (ic, oc, ec) in
  (Buffer.contents obuf), (Buffer.contents ebuf), estatus
 
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
      let cmdstr = Printf.sprintf "stp -sv %s" file in
      let stpout,stperr,pstatus = syscall cmdstr
      in
      
      (* Turn the alarm off *)
      ignore(alarm 0);
      
      let fail = match pstatus with
	| WEXITED(c) ->
	    
	    if c > 0 then (
	      dprintf "FAIL code: %d" c;
	      true 
	    )
	    else false
	      
	| _ -> true
      in
      let isinvalid = ExtString.String.exists stpout "Invalid." in
      let isvalid = ExtString.String.exists stpout "Valid." in
      
      (*   dprintf "fail: %b %b %b" fail isinvalid isvalid; *)
      
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
    Alarm_signal -> 
      (* Another HUGE hack *)
      ignore(Unix.system "killall -9 stp");      
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
