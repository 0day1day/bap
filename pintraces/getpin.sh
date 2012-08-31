#!/usr/bin/env sh

# $Id$
# Download and extract Pin

set -x

# check if pin dir exists first

wget 'http://software.intel.com/sites/landingpage/pintool/downloads/pin-2.12-53271-gcc.4.4.7-ia32_intel64-linux.tar.gz' -O - | tar -xvz -C ..
rm -rf ../pin
mv ../pin-* ../pin
#make
