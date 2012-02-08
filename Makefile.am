# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.

if TESTS_OPT
TESTPOSTDIRS=stpwrap
BAPTESTDIRS=tests
endif

if HAVE_PIN
PINTRACES=pintraces
endif

BAPDIRS=libasmir solvers ocaml utils $(BAPTESTDIRS) $(PINTRACES)
SUBDIRS=zarith-1.0 batteries ounit-1.1.0 pcre-ocaml-release-6.2.2 ocamlgraph-1.8 objsize-0.16 $(BAPDIRS) $(TESTPOSTDIRS)

test: all
	$(MAKE) -C tests test

.PHONY: cscope

cscope:
	cscope -b `find . -name "*.[ch]" -or -name "*.hh" -or -name "*.cc" -or -name "*.cpp"`

.PHONY: bap-clean
bap-clean:
	for d in $(BAPDIRS); do $(MAKE) -C $$d clean; done

all-local: tags
