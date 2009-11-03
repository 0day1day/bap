export BAP_OCAMLDIR=../ocaml
export OCAMLMAKEFILE=$(BAP_OCAMLDIR)/OCamlMakefile

# ocamlfind packages required
export PACKS = str ocamlgraph extlib unix sqlite3
export LIBS = ../ocaml/bap 
export INCDIRS = $(BAP_OCAMLDIR)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR)

# default subprojects to build
ifndef SUBPROJS
	export SUBPROJS = toil iltrans topredicate
endif

define PROJ_toil
	RESULT=toil
	SOURCES = toil.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_toil

define PROJ_iltrans
	RESULT=iltrans
	SOURCES = iltrans.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_iltrans

define PROJ_topredicate
	RESULT=topredicate
	SOURCES = topredicate.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_topredicate


# suffix for bytecode (which when we make all is debug code)
export BCSUFFIX = .dbg

all: nc dc

%:
	make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@


# You may also tell "make" at the command-line what kind of target to
# produce (e.g. "make nc").  Here all the possibilities with shortcuts
# between parenthesis:
# 
#    * byte-code                     (bc)
#    * byte-code-nolink              (bcnl)   - no linking stage
#    * byte-code-library             (bcl)
#    * native-code                   (nc)
#    * native-code-nolink            (ncnl)   - no linking stage
#    * native-code-library           (ncl)
#    * debug-code                    (dc)
#    * debug-code-nolink             (dcnl)   - no linking stage
#    * debug-code-library            (dcl)
#    * profiling-byte-code           (pbc)
#    * profiling-byte-code-library   (pbcl)
#    * profiling-native-code         (pnc)
#    * profiling-native-code-library (pncl)
#    * byte-code-dll                 (bcd)
#    * native-code-dll               (ncd)
#    * pack-byte-code              (pabc)
#    * pack-native-code            (panc)
#    * toplevel interpreter          (top)
#    * subprjs
