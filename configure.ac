# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.


#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Bap, 0.6, bap-dev@lists.andrew.cmu.edu)
AC_CONFIG_SRCDIR([libasmir/src/asm_program.c])
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
# This isn't essential
#AC_PROG_LATEX
#AC_PROG_LIBTOOL

# Check for Z3
AC_ARG_WITH([z3],
            [AS_HELP_STRING([--with-z3],
              [location of Z3 library])],
            [with_z3=$withval],
            [with_z3=no])

# Save pre-z3 ldflags
OLDLDFLAGS=$LDFLAGS

AS_IF([test "x$with_z3" != "xno"],
     [
     LDFLAGS="$OLDLDFLAGS -L$with_z3/lib"
     AC_CHECK_LIB([z3], [Z3_mk_bvadd],
       [
        z3_dir=$with_z3
        with_z3=yes
        AC_DEFINE([HAVE_Z3_BINDINGS], [1], [Define to use Z3 bindings])
        ],

       [AC_MSG_FAILURE([z3 library not found])],
       [-lgmp -lgomp]
       )])

# Restore
LDFLAGS=$OLDLDFLAGS

rm -f solvers/makefile.inc
if `test "x$with_z3" = "xyes"`; then
   cat >>solvers/makefile.inc <<"EOF"
SOLVER_PPFLAGS=-D WITH_Z3_BINDINGS
SOLVER_CLIBS = z3stubs z3
EOF
echo "SOLVER_SOURCES = $z3_dir/ocaml/z3.ml" >>solvers/makefile.inc
echo "SOLVER_LIBDIRS = $z3_dir/lib" >>solvers/makefile.inc
echo "SOLVER_LDFLAGS = -fopenmp -Wl,-rpath=$z3_dir/lib" >> solvers/makefile.inc
else
   touch solvers/makefile.inc
fi

# Add -DAMD64 when needed
if test "$(uname -m)" == "x86_64";
   then export CFLAGS="$CFLAGS -DAMD64";
fi

# Check for PIN
PIN_HOME=${PIN_HOME:=$(pwd)/pin}
AC_CHECK_FILE([$PIN_HOME],
              [HAVE_PIN=true],
              [HAVE_PIN=false; AC_MSG_WARN([Pin not found. BAP pintraces will be unavailable.])])

AM_CONDITIONAL([HAVE_PIN], [test x$HAVE_PIN = xtrue])

# Conditionally build llvm
AS_IF([ocamlfind query llvm],
      [HAVE_LLVM=true],
      [HAVE_LLVM=false; AC_MSG_WARN([LLVM not found. BAP to llvm will be unavailable.])])

rm -f ocaml/makefile.inc
if `test "x$HAVE_LLVM" = "xtrue"` ; then
   cat >>ocaml/makefile.inc <<"EOF"
LLVM_PPFLAGS=-D WITH_LLVM
HAVE_LLVM=true

EOF
else          
   touch ocaml/makefile.inc
fi

# Check for etags
AS_IF([which etags],
      [HAVE_ETAGS=true],
      [HAVE_ETAGS=false; AC_MSG_WARN([ETAGS not found. BAP will not build tags.])])

AM_CONDITIONAL([HAVE_ETAGS], [test x$HAVE_ETAGS = xtrue])

# Checks for ocaml libraries
ocamlfind query camomile
if test $? != 0; then
   echo "bap requires camomile-dev"
   exit -1
fi

#ocamlfind query ocamlgraph
#if test $? != 0; then
#   echo "bap requires libbitstring-ocaml-dev"
#   exit -1
#fi

dnl ocamlfind query ocamlgraph
dnl if test $? != 0; then
dnl     echo "bap needs libocamlgraph-ocaml-dev"
dnl     exit -1
dnl fi
dnl ocamlfind query extlib
dnl if test $? != 0; then
dnl     echo "bap needs libextlib-ocaml-dev"
dnl     exit -1
dnl fi
dnl ocamlfind query batteries
dnl if test $? != 0; then
dnl      echo "bap needs ocaml-batteries-included"
dnl      exit -1
dnl fi
# ocamlfind query sqlite3
# if test $? != 0; then
#     echo "bap needs ocaml-sqlite3 from http://ocaml.info/home/ocaml_sources.html#ocaml-sqlite3"
#     exit -1
# fi

# Where is VEX ?
# stolen from valgrind's configure.in
# Nb: For the 2nd arg, the help string, AS_HELP_STRING is the proper way, but
#     older autoconfs don't support it... here's what it would say:
#
#    AS_HELP_STRING([--with-vex], [Vex directory]),
#


VEX_DIR=`pwd`/'VEX'


dnl AC_ARG_WITH(vex, 
dnl     [  --with-vex=/path/to/vex/dir    Vex directory],
dnl [
dnl     AC_CHECK_FILE($withval/pub/libvex.h,
dnl         [VEX_DIR=$withval],
dnl         [AC_MSG_ERROR([Directory '$withval' does not exist, or does not contain Vex])])
dnl ],
dnl [
dnl ])

dnl if test "${VEX_DIR}" = ""; then
dnl 	echo " ** ERROR: "
dnl 	echo " ** You must supply --with-vex, which should"
dnl 	echo " ** point to the directory containing VEX"
dnl 	exit -1
dnl fi

dnl AC_ARG_WITH(vex-revision, 
dnl     [  --with-vex-revision=<num>    Vex version],
dnl [
dnl         VEX_VERSION=$withval
dnl ],
dnl [
dnl         VEX_VERSION=`svn info ${VEX_DIR} | grep Revision | awk '{print $2}'`
dnl ])
dnl #+

dnl if test -e "${VEX_DIR}/priv/main/vex_svnversion.h"; then
dnl     VEX_VERSION=`tr -d \" <"${VEX_DIR}/priv/main/vex_svnversion.h"`
dnl fi

VEX_VERSION=2201

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
#AC_CHECK_LIB([vex], [LibVEX_Version])

AC_SUBST(VEX_DIR)


#cp libasmir/makefile.inc.in libasmir/makefile.inc
rm -f libasmir/makefile.inc
echo "VEX_DIR=${VEX_DIR}" >> libasmir/makefile.inc
echo "LIBASMIR_CXXFLAGS=-I${VEX_DIR}/pub -I`pwd`/src/include $CXXFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LDFLAGS=-L${VEX_DIR} -L`pwd`/src $LDFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LIBS=-lasmir -lvex -lbfd -ldl -liberty" >> libasmir/makefile.inc
AC_SUBST(ASMIR_DIR)
ASMIR_DIR="`pwd`/libasmir"

AC_SUBST(BAP_DIR)
BAP_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdint.h stdlib.h string.h unistd.h])
AC_CHECK_HEADER([/usr/include/bfd.h], ,
   AC_MSG_ERROR([You need BFD headers (e.g., libbfd-dev)]))
dnl AC_CHECK_HEADER([/usr/include/sqlite3.h], ,
dnl    AC_MSG_ERROR([You need sqlite3 headers (e.g., libsqlite3-dev, and sqlite3 is also recommended but not required)]))


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

#AM_CONDITIONAL([TESTS_OPT], [ test ! -f NOTESTS ])

AC_CONFIG_FILES([Makefile
                 libasmir/Makefile
                 libasmir/apps/Makefile
                 libasmir/src/Makefile
                 libtracewrap/Makefile])
AC_OUTPUT

# Configure ocamlgraph
(cd ocamlgraph-1.8 && ./configure)

# Configure zarith
(cd zarith-1.0 && ./configure)
