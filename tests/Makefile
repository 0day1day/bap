export BAP_OCAMLDIR=../ocaml
export OCAMLMAKEFILE=$(BAP_OCAMLDIR)/OCamlMakefile

export PACKS = oUnit bigarray str ocamlgraph extlib unix
export LIBS = ../ocaml/bap 
export INCDIRS = $(BAP_OCAMLDIR)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR) $(BAP_PROJ_OCAMLDIR)

RESULT = bap_test_suite
#SOURCES = *_suite.ml
SOURCES = \
	il_suite.ml \
	var_suite.ml \
	ast_suite.ml \
	disasm_i386_suite.ml \
	asmir_suite.ml \
	bap_suite.ml

all: x86
#all: nc dc 

# XXX Use ocamlmake to clean up x86 executables?
TRASH = x86/nop

include $(OCAMLMAKEFILE)

# XXX Better way to do this?
x86:
	cd x86 && make

