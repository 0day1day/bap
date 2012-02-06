OCAMLMAKEFILE = ./OCamlMakefile

ROOTDIR = $(CURDIR)/../../..
PIQIBUILDDIR = $(ROOTDIR)/piqi/piqi/build/lib/ocaml

export OCAMLPATH += $(PIQIBUILDDIR)

PACKS = piqi.lib

SOURCES = piqi/frame_piqi.mli \
	  piqi/frame_piqi.ml \
	  trace_container.mli \
	  trace_container.ml
RESULT  = libtrace

all:	ncl dcl

include $(OCAMLMAKEFILE)
