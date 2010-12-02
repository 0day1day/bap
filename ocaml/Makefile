# $Id$

export OCAMLMAKEFILE=OCamlMakefile
export LIBASMIR=../libasmir
include $(LIBASMIR)/makefile.inc
-include ../../ocaml-proj/Makefile

#export INCDIRS += ../stp ../stp/ocaml
#export EXTLIBDIRS += ../stp ../stp/ocaml
#export LIBS += stpvc

#export LIB_PACK_NAME = bap

# common sources
export SOURCES = \
	debug.mli debug.ml \
	util.mli util.ml \
	dominator.ml \
	graphDataflow.mli graphDataflow.ml \
	type.ml type.mli \
	arithmetic.ml \
	var.mli var.ml \
	ast.ml \
	ssa.ml \
	pp.ml \
	typecheck.ml \
	cfg.mli cfg.ml \
	reachable.mli \
	reachable.ml \
	prune_unreachable.mli prune_unreachable.ml \
	cfg_pp.ml \
	cfg_ast.mli cfg_ast.ml \
	ssa_visitor.mli ssa_visitor.ml \
	cfg_ssa.mli cfg_ssa.ml \
	ast_visitor.mli ast_visitor.ml \
	debug_snippets.ml \
	sccvn.mli sccvn.ml \
	deadcode.mli deadcode.ml \
	ssa_simp.ml \
	libbfd.idl \
	libasmir.idl \
	disasm_i386.ml \
	disasm.ml \
	grammar.mly lexer.mll \
	parser.ml \
	memory2array.ml \
	gcl.mli gcl.ml \
	wp.ml \
	stp.ml \
	stpexec.mli stpexec.ml \
	asmirconsts.ml asmir.mli asmir.ml \
	input.mli input.ml \
	depgraphs.ml \
	vsa.ml \
	memory2scalar.ml \
	eval.ml \
	symbeval.ml \
	symbeval_search.ml \
	coalesce.ml \
	traces.ml \
	ast_slice.ml \
	ssa_slice.ml \
	comparelib.ml \
	structural_analysis.ml \
	unroll.ml \
	hacks.ml \
	$(PROJSOURCES) \


RESULT = bap
DOC_FILES := $(filter %.mli, $(SOURCES)) $(filter %.ml, $(SOURCES))



# ocamlfind packages required
export PACKS = str ocamlgraph extlib unix
export YFLAGS = -v


export CLIBS = asmir vex opcodes bfd iberty stdc++ 
export CFLAGS = -g
export LDFLAGS = $(LIBASMIR_LDFLAGS) 
export CLIBFLAGS= $(LDFLAGS)
export RESULTDEPS = $(LIBASMIR)/src/libasmir.a $(VEX_DIR)/libvex.a
export LIBDIRS = $(LIBASMIR)/src # ../appreplay ../stp ../stp/ocaml

OCAMLFLAGS = -warn-error Aelz # -for-pack Bap #-w A
# merge everything but descriptions.
OCAMLDOCFLAGS = -m avlsoper

all: dcl ncl #top
	@echo $(PROJSOURCES)

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

