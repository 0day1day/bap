# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.


#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Bap, 0.1, dbrumley@cmu.edu)
AC_CONFIG_SRCDIR([libasmir/src/asm_program.cpp])
AC_CONFIG_HEADER([libasmir/config.h])

AM_INIT_AUTOMAKE

m4_include([./m4/ac_ocaml.m4])
m4_include([./m4/ac_prog_latex.m4])
#m4_include([./m4/acltx_prog_kpsewhich.m4])
#m4_include([./m4/acltx_package_location.m4])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_PROG_OCAML
AC_PROG_OCAMLFIND
AC_PROG_CAMLIDL
AC_PROG_LATEX
#AC_PROG_LIBTOOL

# Checks for ocaml libraries
ocamlfind query ocamlgraph
if test $? != 0; then
    echo "bap needs libocamlgraph-ocaml-dev"
    exit -1
fi
ocamlfind query extlib
if test $? != 0; then
    echo "bap needs libextlib-ocaml-dev"
    exit -1
fi
ocamlfind query sqlite3
if test $? != 0; then
    echo "bap needs ocaml-sqlite3 from http://ocaml.info/home/ocaml_sources.html#ocaml-sqlite3"
    exit -1
fi

# Where is VEX ?
# stolen from valgrind's configure.in
# Nb: For the 2nd arg, the help string, AS_HELP_STRING is the proper way, but
#     older autoconfs don't support it... here's what it would say:
#
#    AS_HELP_STRING([--with-vex], [Vex directory]),
#
AC_ARG_WITH(vex, 
    [  --with-vex=/path/to/vex/dir    Vex directory],
[
    AC_CHECK_FILE($withval/pub/libvex.h,
        [VEX_DIR=$withval],
        [AC_MSG_ERROR([Directory '$withval' does not exist, or does not contain Vex])])
],
[
    VEX_DIR=`pwd`/'VEX'
])

if test "${VEX_DIR}" = ""; then
	echo " ** ERROR: "
	echo " ** You must supply --with-vex, which should"
	echo " ** point to the directory containing VEX"
	exit -1
fi

AC_ARG_WITH(vex-revision, 
    [  --with-vex-revision=<num>    Vex version],
[
        VEX_VERSION=$withval
],
[
        VEX_VERSION=`svn info ${VEX_DIR} | grep Revision | awk '{print $2}'`
])
+
if test -e "${VEX_DIR}/priv/main/vex_svnversion.h"; then
    VEX_VERSION=`tr -d \" <"${VEX_DIR}/priv/main/vex_svnversion.h"`
fi

if test "${VEX_VERSION}" = ""; then
    echo "Could not determine VEX version (override with --with-vex-revision=<num>)"
    exit -1
fi



#AC_ARG_WITH(binutils, [  --with-binutils=/full/path/to/bintutils])

#if test "${with_binutils}" = ""; then
#	echo " ** ERROR: "
#	echo " ** You must supply --with-binutils, which should"
#	echo " ** point to the directory containing binutils"
#	exit -1
#else
#	CFLAGS="-I$with_binutils/include -I$with_binutils/bfd $CFLAGS"
#	CXXFLAGS="-I$with_binutils/include -I$with_binutils/bfd $CXXFLAGS"
#	LDFLAGS="-L$with_binutils/lib -L$with_binutils/bfd -L$with_binutils/libiberty $LDFLAGS"
#fi


# Checks for libraries.
#AC_LDFLAGS(${VEX_DIR}, ${BINUTILS_DIR})
#echo "HERE: ${AC_LDFLAGS}"
#AC_LIBS([-lasmir], [-lvex], [-lbfd], [-liberty])
#AC_CHECK_LIB([asmir], [libasmir_version])
AC_CHECK_LIB([bfd], [bfd_init])
AC_CHECK_LIB([iberty], [xmalloc])
AC_CHECK_LIB([vex], [LibVEX_Version])

AC_SUBST(VEX_DIR)


cp libasmir/makefile.inc.in libasmir/makefile.inc
echo "VEX_DIR=${VEX_DIR}" >> libasmir/makefile.inc
echo "LIBASMIR_CXXFLAGS=-I${VEX_DIR}/pub -I`pwd`/src/include $CXXFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LDFLAGS=-L${VEX_DIR} -L`pwd`/src $LDFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LIBS=-lasmir -lvex -lbfd -liberty" >> libasmir/makefile.inc


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdint.h stdlib.h string.h unistd.h])
AC_CHECK_HEADER([/usr/include/bfd.h], ,
   AC_MSG_ERROR([You need BFD headers (e.g., libbfd-dev)]))
AC_CHECK_HEADER([/usr/include/sqlite3.h], ,
   AC_MSG_ERROR([You need sqlite3 headers (e.g., libsqlite3-dev, and sqlite3 is also recommended but not required)]))


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_SIZE_T
AC_DEFINE_UNQUOTED([VEX_VERSION], [${VEX_VERSION}], [The VEX SVN Version])

AC_CHECK_SIZEOF(bfd_vma, [], [#include <bfd.h>])

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_CHECK_FUNCS([bzero memset strstr strtoul])


# Checks for latex packages -- to do (without tetex-extra)

# Check for library function versions

AC_CONFIG_FILES([Makefile
                 libasmir/Makefile
                 libasmir/apps/Makefile
                 libasmir/src/Makefile])
AC_OUTPUT
