#!/usr/bin/env bash

apt-get update
apt-get install -y ocaml ocaml-native-compilers ocaml-findlib camlidl binutils-dev automake libcamomile-ocaml-dev otags libpcre3-dev camlp4-extra bison flex zlib1g-dev libgmp3-dev g++ libtool make

cd /bap
./autogen.sh
./configure
make
