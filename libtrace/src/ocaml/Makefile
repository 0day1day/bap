OCAMLMAKEFILE = ./OCamlMakefile

ROOTDIR = $(CURDIR)/../../..
PIQIBUILDDIR = $(ROOTDIR)/piqi/piqi/build/lib/ocaml
BATT = $(ROOTDIR)/batteries/_build/src

export OCAMLPATH = $(PIQIBUILDDIR)

PACKS = piqi.lib

SOURCES = piqi/frame_piqi.mli \
	  piqi/frame_piqi.ml \
	  piqi/frame_piqi_ext.ml \
	  trace_container.mli \
	  trace_container.ml
RESULT  = libtrace

INCDIRS += $(BATT)
LIBS += batteries

all:
	$(MAKE) ncl
	$(MAKE) dcl

include $(OCAMLMAKEFILE)
