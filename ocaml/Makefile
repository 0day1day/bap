# $Id$

export OCAMLMAKEFILE=OCamlMakefile
export LIBASMIR=../libasmir
include $(LIBASMIR)/makefile.inc
#export INCDIRS += ../stp ../stp/ocaml
#export EXTLIBDIRS += ../stp ../stp/ocaml
#export LIBS += stpvc

export LIB_PACK_NAME = bap

# common sources
export SOURCES = \
	debug.mli debug.ml \
	util.mli util.ml \
	dominator.ml \
	graphDataflow.mli graphDataflow.ml \
	type.mli \
	var.mli var.ml \
	ast.ml \
	ssa.ml \
	pp.ml \
	typecheck.ml \
	cfg.mli cfg.ml \
	prune_unreachable.ml \
	cfg_pp.ml \
	cfg_ast.mli cfg_ast.ml \
	ssa_visitor.mli ssa_visitor.ml \
	cfg_ssa.mli cfg_ssa.ml \
	ast_visitor.mli ast_visitor.ml \
	arithmetic.ml \
	sccvn.ml \
	deadcode.ml \
	libasmir.idl \
	asmir.mli asmir.ml \
	grammar.mly lexer.mll \
	parser.ml \
	input.mli input.ml \
	gcl.ml \
	wp.ml \
	stp.ml \



RESULT = bap
DOC_FILES := $(filter %.mli, $(SOURCES)) $(filter %.ml, $(SOURCES))



# ocamlfind packages required
export PACKS = str ocamlgraph extlib unix sqlite3
export YFLAGS = -v


export CLIBS = asmir vex opcodes bfd iberty stdc++ 
export CFLAGS = -g
export LDFLAGS = $(LIBASMIR_LDFLAGS) 
export CLIBFLAGS= $(LDFLAGS)
export LIBDIRS = $(LIBASMIR)/src # ../appreplay ../stp ../stp/ocaml

OCAMLFLAGS = -warn-error Aelz  -for-pack Bap #-w A
# merge everything but descriptions.
OCAMLDOCFLAGS = -m avlsoper

all: dcl ncl #top

bap_lib_doc: all htdoc


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



include $(OCAMLMAKEFILE)

bap.dot: $(DOC_FILES)
	 ocamlfind ocamldoc -I /usr/lib/ocaml/3.10.2/ -package str,ocamlgraph,extlib,unix,sqlite3 -dot -o bap.dot -m avlsoper $(DOC_FILES)
