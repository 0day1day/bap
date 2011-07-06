export BAP_OCAMLDIR=../ocaml
export OCAMLMAKEFILE=$(BAP_OCAMLDIR)/OCamlMakefile

export PACKS = oUnit bigarray str ocamlgraph extlib unix
export LIBS = ../ocaml/bap 
export INCDIRS = $(BAP_OCAMLDIR)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR) $(BAP_PROJ_OCAMLDIR)

TARGETS = ASM nc dc

RESULT = bap_test_suite

SOURCES = \
	il_suite.ml \
	var_suite.ml \
	ast_suite.ml \
	disasm_i386_suite.ml \
	asmir_suite.ml \
	bap_suite.ml

all: $(TARGETS)

# XXX Use ocamlmake to clean up x86 executables?
TRASH = x86/nop

include $(OCAMLMAKEFILE)

# XXX Better way to do this?
ASM:
	cd ./x86 ; make

