# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.

#include @top_srcdir@/config/recursive.mk

SUBDIRS=ounit-1.1.0 pcre-ocaml-release-6.2.2 bigint-3.12 batteries-1.4.0 VEX libasmir ocaml utils tests pintraces stpwrap
# stp/ocaml ocaml utils doc

test: all
	cd tests/ ; ./bap_test_suite

cscope:
	cscope -b `find . -name "*.[ch]" -or -name "*.hh" -or -name "*.cc" -or -name "*.cpp"`

#regression:
#	cd $(top_srcdir)/../regression/ocaml;./test.sh
