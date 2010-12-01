# Project specific utils Makefile
# $Id$

# We need to tell the main utils Makefile to include our proj's ocaml
export BAP_PROJ_OCAMLDIR=../../ocaml-proj
export BAP_PROJ_UTILDIR=../../utils-proj

define PROJ_testutil
	RESULT=testutil
	SOURCES = $(BAP_PROJ_UTILDIR)/testutil.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_testutil

export PROJUTILS = testutil
