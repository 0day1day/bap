bin_PROGRAMS = \
	print-ir

print_ir_SOURCES = print-ir.cpp ir_printer.cpp
LDADD =  -lasmir -lvex -lbfd -liberty -lopcodes
AM_LDFLAGS = -L../src -L@VEX_DIR@ -L@BINUTILS_DIR@
AM_CXXFLAGS = -I../include  -I@VEX_DIR@/pub -I@BINUTILS_DIR@

# these executables should depend on libasmir, since they link it in
# statically.
$(bin_PROGRAMS): ../src/libasmir.a
