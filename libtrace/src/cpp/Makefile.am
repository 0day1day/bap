AM_CXXFLAGS = -I./pb -I@BAP_DIR@/protobuf/src
AM_CXXFLAGS += -m32

lib_LIBRARIES = libtrace.a
libtrace_a_SOURCES = pb/frame.piqi.pb.h pb/frame.piqi.pb.cc trace.container.cpp

utils_LDADD = libtrace.a @BAP_DIR@/protobuf/src/.libs/libprotobuf.a -lpthread

bin_PROGRAMS = readtrace copytrace
readtrace_SOURCES = utils/readtrace.cpp
readtrace_LDADD = $(utils_LDADD)
copytrace_SOURCES = utils/copytrace.cpp
copytrace_LDADD = $(utils_LDADD)

# Don't auto-link LIBS coming from autoconf, because we might not have
# 32-bit equivalents!
LIBS =