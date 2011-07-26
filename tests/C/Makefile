
SRC=$(wildcard *.c)
EXECS = $(SRC:%.c=%)
CFLAGS = -m32 -static

all: $(EXECS)

$(EXECS): $(SRC)
	gcc $@.c -o $@ $(CFLAGS)

clean:
	rm -f $(EXECS)