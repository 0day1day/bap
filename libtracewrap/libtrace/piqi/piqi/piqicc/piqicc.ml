(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
(*
   Copyright 2009, 2010, 2011 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


(*
 * piq interface compiler compiler
 *)


module C = Piqi_common
open Piqi_common
open Iolist


let read_file fname =
  let ch = Pervasives.open_in_bin fname in
  let len = Pervasives.in_channel_length ch in
  let res = String.create len in
  Pervasives.really_input ch res 0 len;
  close_in ch;
  res


let embed_module ch ?fname piqi =
  let modname = some_of piqi.P#modname in
  let fname =
    match fname with
      | Some x -> x
      | None ->
          (* TODO: store file name in the Piqi structure while loading *)
          Piqi_file.find_piqi modname
  in
  let content = read_file fname in
  let content = String.escaped content in
  let code = iol [
    ios "let _ = add_embedded_piqi ";
    ios "("; ioq modname; ios ", "; ioq content; ios ")";
    eol;
  ]
  in
  Iolist.to_channel ch code


let embed_boot_modules ch boot_fname =
  debug "embded boot modules(0)\n";
  let code = iod " " [
      (* the list of embedded modules *)
      ios "let embedded_piqi = ref []";
      ios "let add_embedded_piqi x = embedded_piqi := x :: !embedded_piqi";
      eol;
  ]
  in
  Iolist.to_channel ch code;

  let boot_piqi = some_of !C.boot_piqi in
  (* the list of all included modules including the current one *)
  let modules = boot_piqi.P#included_piqi in
  (* Embed in a way that the root module will be at the end of the list and all
   * dependencies are added before *)
  embed_module ch boot_piqi ~fname:boot_fname;
  List.iter (embed_module ch) (List.tl (List.rev modules))


(* piq interface compiler compile *)
let piqicc ch boot_fname piqi_fname piqi_impl_fname =
  trace "piqicc(0)\n";
  (* reload the boot module from file if it was specified using --boot option *)
  Piqi.load_boot_piqi boot_fname;

  trace "piqicc: loading piqi spec from: %s\n" piqi_fname;
  let piqi = Piqi.load_piqi piqi_fname in

  trace "piqicc: loading piqi-impl spec from: %s\n" piqi_impl_fname;
  let piqi_impl = Piqi.load_piqi piqi_impl_fname in

  trace "piqicc: piqi compiling compiler\n";
  (* TODO,XXX:
    * report invalid module name?
    * check & report piqi incompatibility
  *)
  let boot_piqi = some_of !C.boot_piqi in

  (* prepare embedded Piqi spec *)
  let piqi = P#{
    (some_of piqi.original_piqi) with
      (* using piqi.org/piqtype instead of piqi.org/piqi to generate hashcodes
       * otherwise, serial wire codes would be generated *)
      modname = Some "piqi.org/piqtype";
      ocaml_module = None; (* XXX *)

      (* unresolved, but expanded piqdef list *)
      piqdef = piqi.P#extended_piqdef;
      includ = [];
      import = [];
      extend = [];

      (* NOTE: leaving the original custom_fields *)
      (*
      custom_field = [];

      extended_piqdef = [];
      resolved_piqdef = [];
      imported_piqdef = [];
      resolved_import = [];
      included_piqi = [];
      original_piqi = None;
      *)
  }
  in
  (* prepare embedded Piqi spec *)
  let boot_piqi = P#{
    (some_of boot_piqi.original_piqi) with
      (* using piqi.org/piqtype instead of piqi.org/piqi to generate hashcodes
       * otherwise, serial wire codes would be generated *)
      modname = Some "piqi.org/piqtype";
      ocaml_module = None; (* XXX *)

      (* unresolved, but expanded piqdef list *)
      piqdef = boot_piqi.P#extended_piqdef;
      includ = [];
      import = [];
      extend = [];

      (* NOTE: leaving the original custom_fields *)
      (*
      custom_field = [];
      *)
  }
  in
  let gen_piqi_binobj piqi = Piqirun.gen_binobj T.gen__piqi piqi in

  let piqi_binobj = gen_piqi_binobj piqi in
  let piqi_boot_binobj = gen_piqi_binobj boot_piqi in

  let code = iod " " [
    ios "let parse_piqi_binobj x = ";
      ios "Piqirun.parse_binobj parse_piqi x";
    eol;

    ios "let piqi = ";
      ios "let piqi_binobj = "; ioq (String.escaped piqi_binobj);
      ios "in parse_piqi_binobj piqi_binobj";
    eol;

    ios "let boot_piqi = ";
      ios "let piqi_boot_binobj = "; ioq (String.escaped piqi_boot_binobj);
      ios "in parse_piqi_binobj piqi_boot_binobj";
    eol;
  ]
  in
  (* call piq interface compiler for ocaml *)
  (* TODO: move it to Piqic_config module *)
  Piqic_ocaml_types.cc_mode := true;
  (* generate default values for generated OCaml types *)
  Piqic_common.flag_gen_defaults := true;
  (* Override supplied module name *)
  let piqi_impl = P#{piqi_impl with ocaml_module = Some "Piqtype"} in
  let code = iol [
    Piqic_ocaml_base.piqic piqi_impl;
    code;
  ]
  in
  Iolist.to_channel ch code;

  embed_boot_modules ch boot_fname


module Main = Piqi_main
open Main


(* command-line options *)
let boot_file = ref ""
let piqi_file = ref ""
let piqi_impl_file = ref ""


let usage = "Usage: piqicc --boot ... --piqi ... --impl ...\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;
    (* XXX: arg_C; *)
    "--boot", Arg.Set_string boot_file,
      "<.piqi file> specify a Piqi boot module";
    "--piqi", Arg.Set_string piqi_file,
      "<.piqi file> specify the Piqi language spec";
    "--impl", Arg.Set_string piqi_impl_file,
      "<.piqi file> specify spec for internal representation";
  ]


let piqicc_file () =
  let error s =
    Printf.eprintf "Error: %s\n\n" s;
    Arg.usage speclist usage;
    die ""
  in
  if !piqi_file = "" then error "'--piqi' parameter is missing";
  if !piqi_impl_file = "" then error "'--impl' parameter is missing";
  if !boot_file = "" then error "'--boot' parameter is missing";

  let ch = Main.open_output !ofile in
  piqicc ch !boot_file !piqi_file !piqi_impl_file


let run () =
  Main.parse_args () ~usage ~speclist ~min_arg_count:0 ~max_arg_count:0;
  piqicc_file ()

 
let _ =
  Main.register run "piqi compiler compiler"

