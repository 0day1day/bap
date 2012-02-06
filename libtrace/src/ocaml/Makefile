OCAMLMAKEFILE = ./OCamlMakefile

ROOTDIR = $(CURDIR)/../../..

PIQILIBDIR = $(ROOTDIR)/piqi/piqi/build/lib/ocaml/piqirun
INCDIRS += $(PIQILIBDIR)
LIBDIRS += $(PIQILIBDIR)

LIBS = piqirun
SOURCES = piqi/frame_piqi.mli \
	  piqi/frame_piqi.ml \
	  trace_container.mli
RESULT  = libtrace

all:	ncl dcl

include $(OCAMLMAKEFILE)
