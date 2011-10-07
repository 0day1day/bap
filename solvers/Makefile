# This code was borrowed from the PIN Makefile.
ifndef HOST_ARCH
    # default to building for the host you are on. You can override this on the cmd line.
    HST=$(shell uname -m)
    ifeq (${HST},x86_64)
        HOST_ARCH=ia32e
    endif
    ifeq (${HST},amd64)
        HOST_ARCH=ia32e
    endif
    ifeq (${HST},i686)
        HOST_ARCH=ia32
    endif
    ifeq (${HST},x86)
        HOST_ARCH=ia32
    endif
    ifeq (${HST},i386)
        HOST_ARCH=ia32
    endif
endif

ifndef HOST_ARCH
    $(error could not detect building host. please define HOST_ARCH on the command line)       
endif

all:	z3

.PHONY: clean
clean:

.PHONY:	z3
z3:
	rm -rf z3
ifeq (${HOST_ARCH},ia32)
	ln -s z3-x86 z3
else
	ln -s z3-x64 z3
endif
	make -C z3/ocaml
