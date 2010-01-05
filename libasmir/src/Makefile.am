
RANLIB = @RANLIB@
lib_LIBRARIES = libasmir.a
#lib_LTLIBRARIES = libasmir.la
AM_CFLAGS = -I@VEX_DIR@/pub -I../include -fPIC
AM_CXXFLAGS = $(AM_CFLAGS)


libasmir_a_SOURCES = \
	./old_ir/stmt.cpp \
	./old_ir/stmt_c.cpp \
	./old_ir/exp.cpp \
	./old_ir/exp_c.cpp \
	asm_program.c \
	memory_data.cpp \
	irtoir_c.cpp \
	irtoir.cpp \
	irtoir-i386.cpp \
	irtoir-arm.cpp \
	vexir.c \
	vexmem.c 

