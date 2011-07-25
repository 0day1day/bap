open OUnit
open Pcre

let startup dir _ =
	let pwd = Sys.getcwd () in
	Sys.chdir dir;
	pwd;;


let chdir_cleanup pwd = Sys.chdir pwd;;

let suite = "Pin" >:::
  [
	"pin_dir_test" >:: 
	  (fun () ->
		(@?) "Directory ../pin does not exist!" (Sys.is_directory "../pin"));
	"pin_obj_test" >:: bracket 
	  (startup "../pintraces/obj-ia32/") 
	  (fun _ -> 
		(@?) "File gentrace.so does not exist!" (Sys.file_exists "gentrace.so"))
	  chdir_cleanup;
  ]
