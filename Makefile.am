# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.

#include @top_srcdir@/config/recursive.mk

SUBDIRS=VEX libasmir ocaml utils tests
# stp/ocaml ocaml utils doc

test: all
	cd tests/ ; ./bap_test_suite

#regression:
#	cd $(top_srcdir)/../regression/ocaml;./test.sh
