.NOPARALLEL:

export OCAMLMAKEFILE = ../OCamlMakefile

ROOTDIR = $(realpath $(CURDIR)/../../../..)
PIQIBUILDDIR = $(ROOTDIR)/piqi/piqi/build/lib/ocaml
BATT = $(ROOTDIR)/batteries/_build/src
LIBTRACEDIR = $(realpath ..)
ZARITHDIR = $(ROOTDIR)/zarith-1.0

export BCSUFFIX = .dbg

export OCAMLPATH = $(PIQIBUILDDIR)

export PACKS = piqi.lib unix camomile num

export INCDIRS += $(BATT)
export INCDIRS += $(LIBTRACEDIR) $(LIBTRACEDIR)/piqi
export INCDIRS += $(ZARITHDIR)

export LIBDIRS += $(LIBTRACEDIR)
#export LIBDIRS += $(ZARITHDIR)

export LIBS += batteries zarith libtrace

define PROJ_readtrace
	SOURCES = readtrace.ml
	RESULT = readtrace
endef
export PROJ_readtrace

define PROJ_copytrace
	SOURCES = copytrace.ml
	RESULT = copytrace
endef
export PROJ_copytrace

ifndef SUBPROJS
	export SUBPROJS = readtrace copytrace
endif

all:	nc dc

%:
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
