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
