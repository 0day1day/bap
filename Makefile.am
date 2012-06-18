if HAVE_PIN
PINTRACES=pintraces
endif


BAPDIRS=libasmir ocaml utils tests $(PINTRACES)
SUBDIRS=batteries zarith-1.0 ounit-1.1.0 pcre-ocaml ocamlgraph-1.8 objsize-0.16 libtracewrap $(BAPDIRS) stpwrap

test: all
	$(MAKE) -C tests test

.PHONY: cscope

cscope:
	cscope -b `find . -name "*.[ch]" -or -name "*.hh" -or -name "*.cc" -or -name "*.cpp"`

.PHONY: bap-clean
bap-clean:
	for d in $(BAPDIRS); do $(MAKE) -C $$d clean; done

all-local: tags
