#!/bin/sh
#
#  Build and install 'koshu' inside sandbox
#

check () {
    if [ "$1" = 0 ]; then
        shift
        echo "ok - $*"
    else
        echo "ABORT - $*"
        exit 2
    fi
}

cabal sandbox init
  check $? init

for pkg in base data core writer operator cop content; do
    cabal sandbox add-source ../$pkg
    check $? $pkg
done

cabal install --only-dependencies
  check $? install --only-dependencies
cabal install
  check $? install
