include Makefile.defs

CFLAGS = -m32 -static

all: $(CEXECS)

$(CEXECS): $(CSRC)
	gcc $@.c -o $@ $(CFLAGS)

clean:
	rm -f $(CEXECS)