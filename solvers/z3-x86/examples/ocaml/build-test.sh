#!/bin/bash

OCAMLLIB=$1
 
ocamlopt -dtypes -o test_mlapi -ccopt "-I../../ocaml -L../../bin -L../../lib" -I ../../ocaml/ -cclib -lz3 -cclib -lz3stubs /usr/lib/ocaml/libcamlidl.a z3.cmxa test_mlapi.ml
