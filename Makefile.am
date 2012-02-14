if TESTS_OPT
TESTPOSTDIRS=stpwrap
BAPTESTDIRS=tests
endif

if HAVE_PIN
PINTRACES=pintraces
endif

BAPDIRS=libasmir solvers ocaml utils $(BAPTESTDIRS) $(PINTRACES)
SUBDIRS=batteries zarith-1.0 ounit-1.1.0 pcre-ocaml-release-6.2.2 ocamlgraph-1.8 objsize-0.16 libtrace VEX $(BAPDIRS) $(TESTPOSTDIRS)

test: all
	$(MAKE) -C tests test

.PHONY: cscope

cscope:
	cscope -b `find . -name "*.[ch]" -or -name "*.hh" -or -name "*.cc" -or -name "*.cpp"`

.PHONY: bap-clean
bap-clean:
	for d in $(BAPDIRS); do $(MAKE) -C $$d clean; done

all-local: tags
