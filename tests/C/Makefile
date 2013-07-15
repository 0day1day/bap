include Makefile.defs

CFLAGS = -m32 -fno-stack-protector

all: $(CEXECS)

recover-hard-opt: recover-hard.c
	$(CC) $< -o $@ $(CFLAGS) -O3

#$(CEXECS): $(CSRC)
#	gcc $@.c -o $@ $(CFLAGS)

clean:
	rm -f $(CEXECS)
