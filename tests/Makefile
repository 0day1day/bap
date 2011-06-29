export BAP_OCAMLDIR=../ocaml
export OCAMLMAKEFILE=$(BAP_OCAMLDIR)/OCamlMakefile

export PACKS = oUnit bigarray str ocamlgraph extlib unix
export LIBS = ../ocaml/bap 
export INCDIRS = $(BAP_OCAMLDIR)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR) $(BAP_PROJ_OCAMLDIR)

RESULT = bap_suite
SOURCES = \
	il_suite.ml \
	var_suite.ml \
	bap_suite.ml


all: nc dc

include $(OCAMLMAKEFILE)