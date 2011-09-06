# XXX Fix this so it actually works
include C/Makefile.defs

export BAP_OCAMLDIR=../ocaml
export OCAMLMAKEFILE=$(BAP_OCAMLDIR)/OCamlMakefile
export BIGINT=$(CURDIR)/../bigint-3.12/otherlibs/num
export BATT=$(CURDIR)/../batteries-1.4.0/_build/src
export OUNIT=$(CURDIR)/../ounit-1.1.0/_build/src/
export PCRE=$(CURDIR)/../pcre-ocaml-release-6.2.2/lib

# Stop annoying warnings about big_int duplication!
export OCAMLFIND_IGNORE_DUPS_IN=$(CURDIR)/../bigint-3.12/otherlibs/num

#export PACKS = oUnit bigarray str ocamlgraph extlib unix
export PACKS = bigarray str ocamlgraph unix camomile
export LIBS = nums batteries ../ocaml/bap oUnit pcre
export INCDIRS = $(BAP_OCAMLDIR) $(BIGINT) $(BATT) $(OUNIT) $(PCRE)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR) $(BAP_PROJ_OCAMLDIR)

ifndef SUBPROJS
	export SUBPROJS = analyze_test_log
endif

CSRC=$(wildcard C/*.c)
CEXECS = $(CSRC:C/%.c=%)


TARGETS = CTESTS ASM nc dc 

RESULT = bap_test_suite

# XXX Use ocamlmake to clean up asm executables?
TRASH = asm/nop C/test C/taint_test C/bof1

SOURCES = \
	testCommon.ml \
	il_suite.ml \
	var_suite.ml \
	ast_suite.ml \
	disasm_i386_suite.ml \
	asmir_suite.ml \
	traces_suite.ml \
	pin_suite.ml \
	predicate_suite.ml \
	large_binary_consistency_suite.ml \
	arithmetic_suite.ml \
	bap_suite.ml

# XXX How to get analyze_test_log to be cleaned?
define PROJ_analyze_test_log
	RESULT=analyze_test_log
	SOURCES = analyze_test_log.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_analyze_test_log

export BCSUFFIX = .dbg

all: $(TARGETS)

test: all
	./$(RESULT)

include $(OCAMLMAKEFILE)

ASM:
	make -C asm

CTESTS:
	make -C C

%:
	make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@