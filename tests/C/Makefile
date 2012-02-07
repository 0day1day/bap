include Makefile.defs

CFLAGS = -m32 -static -fno-stack-protector

all: $(CEXECS)

$(CEXECS): $(CSRC)
	gcc $@.c -o $@ $(CFLAGS)

clean:
	rm -f $(CEXECS)