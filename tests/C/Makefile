include Makefile.defs

# XXX: Use exported variable from configure.ac (LIBTRACE_X64)
ARCH = $(shell getconf LONG_BIT)

CFLAGS = -fno-stack-protector

all: $(CEXECS)

$(CEXECS): $(CSRC)
	gcc -m32 $@.c -o $@ $(CFLAGS)
ifeq ($(ARCH), 64)
	gcc $@.c -o $@_64 $(CFLAGS)
endif


clean:
	rm -f $(CEXECS)
ifeq ($(ARCH), 64)
	rm -f $(CEXECS_64)
endif
