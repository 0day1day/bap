# Makefile for AEG experiment

PACKAGES=bof1

all: $(PACKAGES)

bof1: bof1.c
	gcc -o $@ -fno-stack-protector -m32 -mpreferred-stack-boundary=2 $^

.PHONY: clean all

clean:
	rm -rf *.o $(PACKAGES)
