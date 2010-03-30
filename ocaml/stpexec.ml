(**
   Module for executing STP inside of BAP

   This is a pretty big hack, and is not portable in any shape or way.
*)

module D = Debug.Make(struct let name = "Stpexec" and default=`Debug end)
open D

open Unix

type result = Valid | Invalid | Timeout

let result_to_string result =
  match result with
  | Valid -> "Valid"
  | Invalid -> "Invalid"
  | Timeout -> "Timeout"

let stp = "/home/ed/f09/stp-ver-0.1-11-18-2008/bin/stp"
let sh = "/bin/bash"

let runstp ?(timeout=60) file =
  let pid = fork () in
  match pid with
  | 0 -> ((* child *)      
(*       ignore(alarm timeout); *)
      (* Yes, this is the biggest hack ever. *)
      let cmdstr = "perl -e 'alarm shift @ARGV; exec @ARGV' " ^ string_of_int timeout ^ " sh -c 'stp -sv " ^ file ^ " | grep Valid'" in
      dprintf "Cmdstr: %s" cmdstr;
      ignore(Unix.execv sh [| "sh"; "-c"; cmdstr |]);
      
      exit 2
    )

  | cpid -> ((* parent *)
      let (pid,status) = (* waitpid [] cpid *) wait () in
      dprintf "Pid %d exited" pid;

      (match status with
      | WEXITED(code) -> dprintf "exit: %d" code
      | WSIGNALED(signum) -> dprintf "signal: %d" signum
      | _ -> dprintf "Wtf");


      match status with
      | WEXITED(code) when code = 0 -> Valid
      | WEXITED(code) when code = 1 -> Invalid
      | WSIGNALED(signum) when signum = Sys.sigalrm -> Timeout
      | _ -> failwith "Unexpected termination"

    )
