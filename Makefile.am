# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.

if TESTS_OPT
TESTPOSTDIRS=stpwrap tests
endif

if HAVE_PIN
PINTRACES=pintraces
endif

SUBDIRS=bigint-3.12 batteries-1.4.0 ounit-1.1.0 pcre-ocaml-release-6.2.2 objsize-0.16 VEX libasmir solvers ocaml utils $(TESTPOSTDIRS) $(PINTRACES)

test: all
	$(MAKE) -C tests test

.PHONY: cscope

cscope:
	cscope -b `find . -name "*.[ch]" -or -name "*.hh" -or -name "*.cc" -or -name "*.cpp"`

all-local: tags
