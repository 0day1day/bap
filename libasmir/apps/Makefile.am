bin_PROGRAMS = \
	print-ir \
	lifttrace

print_ir_SOURCES = print-ir.cpp ir_printer.cpp
lifttrace_SOURCES = lifttrace.cpp
LDADD =  -lasmir -lvex -lbfd -ldl -liberty -lopcodes
AM_LDFLAGS = -L../src -L@VEX_DIR@ -L@BINUTILS_DIR@
AM_CXXFLAGS = \
              -I../include \
              -I../src/traces \
              -I@VEX_DIR@/pub -I@BINUTILS_DIR@

# these executables should depend on libasmir, since they link it in
# statically.
$(bin_PROGRAMS): ../src/libasmir.a
