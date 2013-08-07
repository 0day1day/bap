# Makefile for AEG experiment

PACKAGES=bof1

all: $(PACKAGES)

bof1: bof1.c
	gcc -m32 -o $@ -fno-stack-protector -mpreferred-stack-boundary=2 $^

bof1-64: bof1.c
	gcc -o $@ -fno-stack-protector $^

.PHONY: clean all

clean:
	rm -rf *.o $(PACKAGES)
