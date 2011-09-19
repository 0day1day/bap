# XXX Fix this so it actually works
include C/Makefile.defs

export BAP_DIR=$(CURDIR)/..

-include $(BAP_DIR)/../tests-proj/Makefile

export BAP_OCAMLDIR=$(BAP_DIR)/ocaml
export OCAMLMAKEFILE=$(BAP_OCAMLDIR)/OCamlMakefile
export BIGINT=$(BAP_DIR)/bigint-3.12/otherlibs/num
export BATT=$(BAP_DIR)/batteries-1.4.0/_build/src
export OUNIT=$(BAP_DIR)/ounit-1.1.0/_build/src/
export PCRE=$(BAP_DIR)/pcre-ocaml-release-6.2.2/lib

# Stop annoying warnings about big_int duplication!
export OCAMLFIND_IGNORE_DUPS_IN=$(BAP_DIR)/bigint-3.12/otherlibs/num

#export PACKS = oUnit bigarray str ocamlgraph extlib unix
export PACKS = bigarray str ocamlgraph unix camomile
export LIBS = nums batteries oUnit pcre ../ocaml/bap
export INCDIRS = $(BAP_OCAMLDIR) $(BIGINT) $(BATT) $(OUNIT) $(PCRE)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR) $(BAP_PROJ_OCAMLDIR)

export SUBPROJS += $(PROJTESTS) analyze_test_log

CSRC=$(wildcard C/*.c)
CEXECS = $(CSRC:C/%.c=%)


TARGETS = CTESTS ASM nc dc 

RESULT = bap_test_suite

# XXX Use ocamlmake to clean up asm executables?
TRASH = asm/nop C/test C/taint_test C/bof1

SOURCES = \
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

tags: TAGS

TAGS: $(SOURCES)
	-otags $^

%:
	make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
