aclocal
autoconf
autoheader
automake --add-missing --copy
(cd libtracewrap/libtrace && ./autogen.sh)
(cd ocamlgraph-1.8 && autoconf)
