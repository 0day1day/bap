#!/bin/bash

# Thorough cleaning
git clean -f -x -d || exit 125

# If PINPATH is set, copy pin into the directory.
if [ "x$PINPATH" != "x" ]
then
cp -r $PINPATH . || exit 125
fi

# Build bap
./autogen.sh && ./configure && make -j

# A build error is not considered 'bad'
if [ $? -ne 0 ]
then
    exit 125
fi

#make -j test | grep -v "Trace should be satisfiable but is unsatisfiable"

make -j test
RESULT=$?

git clean -f -x -d || exit 125

exit $RESULT