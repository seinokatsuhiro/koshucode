#!/bin/sh
#
#  Build and install 'koshu' inside sandbox
#

install_pwd=`pwd`
install_dir=`basename $install_pwd`

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

for pkg in `sh ../koshu-pkg.sh dir`; do
    if [ $pkg = $install_dir ]; then
        break
    fi
    cabal sandbox add-source ../$pkg
    check $? $pkg
done

cabal install --only-dependencies
  check $? install --only-dependencies
cabal install
  check $? install
