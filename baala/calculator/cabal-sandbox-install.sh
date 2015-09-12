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

cabal sandbox add-source ../base
  check $? base
cabal sandbox add-source ../core
  check $? core
cabal sandbox add-source ../writer
  check $? writer
cabal sandbox add-source ../operator
  check $? operator
cabal sandbox add-source ../content
  check $? content

cabal install --only-dependencies
  check $? install --only-dependencies
cabal install
  check $? install
