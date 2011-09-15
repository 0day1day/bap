export BAP_DIR=$(CURDIR)/../bap
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
export LIBS = nums batteries bap oUnit pcre
export INCDIRS = $(BAP_OCAMLDIR) $(BIGINT) $(BATT) $(OUNIT) $(PCRE)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR) $(BAP_PROJ_OCAMLDIR)

TARGETS = nc dc

RESULT = project_test_suite

SOURCES = \
	project_suite.ml

export BCSUFFIX = .dbg

all: $(TARGETS)

test: all
	./$(RESULT)

include $(OCAMLMAKEFILE)

tags: TAGS

TAGS: $(SOURCES)
	-otags $^

%:
	make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
