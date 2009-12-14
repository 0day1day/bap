bin_PROGRAMS = \
	lifttrace

lifttrace_SOURCES = \
		trace.cpp \
        trace_v40.cpp \
        trace_v41.cpp \
        trace_v50.cpp \
        reg_mapping.cpp \
        readtrace.cpp \
        main.cpp
LDADD =  -lasmir -lvex -lbfd -liberty -lopcodes
AM_LDFLAGS = -L../ -L@VEX_DIR@ -L@BINUTILS_DIR@
AM_CXXFLAGS = -I../../include -I../../apps -I@VEX_DIR@/pub -I@BINUTILS_DIR@

# no recompilation when libasmir.a changes - ethan
#$(bin_PROGRAMS): ../libasmir.a
