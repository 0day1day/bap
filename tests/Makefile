.NOTPARALLEL:

# XXX Fix this so it actually works
include C/Makefile.defs

export BAP_DIR=$(CURDIR)/..

.PHONY: $(BAP_DIR)/../tests-proj/Makefile
-include $(BAP_DIR)/../tests-proj/Makefile

export BAP_OCAMLDIR=$(BAP_DIR)/ocaml
export OCAMLMAKEFILE=$(BAP_OCAMLDIR)/OCamlMakefile
export BIGINT=$(BAP_DIR)/zarith-1.0
export BATT=$(BAP_DIR)/batteries/_build/src
export OUNIT=$(BAP_DIR)/ounit-1.1.0/_build/src/
export PCRE=$(BAP_DIR)/pcre-ocaml/lib
export OCAMLGRAPH=$(BAP_DIR)/ocamlgraph-1.8
export OBJSIZE=$(BAP_DIR)/objsize-0.16
export LIBTRACE=$(BAP_DIR)/libtracewrap/libtrace/src/ocaml
export PIQIBUILDDIR=$(BAP_DIR)/libtracewrap/libtrace/piqi/piqi/build/lib/ocaml

# Stop annoying warnings about big_int duplication!
#export OCAMLFIND_IGNORE_DUPS_IN=$(BAP_DIR)/bigint-3.12/otherlibs/num

#export PACKS = oUnit bigarray str ocamlgraph extlib unix
export PACKS = bigarray str num unix camomile piqi.lib
export LIBS = zarith batteries oUnit pcre objsize graph libtrace bap
export INCDIRS = $(BAP_OCAMLDIR) $(BIGINT) $(BATT) $(OUNIT) $(PCRE) $(OCAMLGRAPH) $(OBJSIZE) $(LIBTRACE)
export OCAMLPATH = $(PIQIBUILDDIR)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR) $(BAP_PROJ_OCAMLDIR)

export SUBPROJS += $(PROJTESTS) analyze_test_log baptest

CSRC=$(wildcard C/*.c)
CEXECS = $(CSRC:C/%.c=%)

# XXX Use ocamlmake to clean up asm executables?
TRASH = asm/nop C/test C/taint_test C/bof1

TARGETS = CTESTS ASM nc dc

define PROJ_baptest
	RESULT = bap_test_suite
	SOURCES = \
	il_suite.ml \
	var_suite.ml \
	ast_suite.ml \
	disasm_i386_suite.ml \
	asmir_suite.ml \
	traces_suite.ml \
	pin_suite.ml \
	predicate_suite.ml \
	arithmetic_suite.ml \
	bap_suite.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_baptest

define PROJ_analyze_test_log
	RESULT=analyze_test_log
	SOURCES = analyze_test_log.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_analyze_test_log

export BCSUFFIX = .dbg

all: $(TARGETS)

test: all
	./bap_test_suite

ASM:
	make -C asm

CTESTS:
	make -C C

.PHONY: tags
tags: TAGS

.PHONY: TAGS
TAGS:
	-otags $^

debug: dc
	@echo ocamldebug -I $(BIGINT) -I $(BAP_OCAMLDIR) -I $(BATT) -I $(OUNIT) -I $(PCRE) -I $(OBJSIZE) program

%:
	make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

