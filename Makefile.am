# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.

#include @top_srcdir@/config/recursive.mk

if TESTS_OPT
TESTPOSTDIRS=stpwrap
# BAPTESTDIRS=tests pintraces
BAPTESTDIRS=tests
endif

BAPDIRS=libasmir solvers ocaml utils $(BAPTESTDIRS)
SUBDIRS=bigint-3.12 batteries-1.4.0 ounit-1.1.0 pcre-ocaml-release-6.2.2 objsize-0.16 VEX $(BAPDIRS) $(TESTPOSTDIRS)

test: all
	$(MAKE) -C tests test


.PHONY: bap-clean cscope

bap-clean: 
	-for d in $(BAPDIRS); do $(MAKE) -C $$d clean; done

cscope:
	cscope -b `find . -name "*.[ch]" -or -name "*.hh" -or -name "*.cc" -or -name "*.cpp"`

#regression:
#	cd $(top_srcdir)/../regression/ocaml;./test.sh

all-local: tags
