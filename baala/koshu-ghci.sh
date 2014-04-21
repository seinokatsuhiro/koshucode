#!/bin/sh

help () {
    echo "DESCRIPTION"
    echo "  Invoke Haskell REPL for Koshucode"
    echo
    echo "NOTE"
    echo "  Please move to the package directory of koshucode,"
    echo "  or make a symbolic link for koshucode-master"
    echo "  in the directory: $PROG_DIR"
}

pkg_dir () {
    pwd | sed -n 's:\(.*/koshucode-master/baala/[a-z]*\).*:\1:p'
}

cabal_repl () {
    echo "-- Repl in $1"
    cd "$1"
    cabal repl
}

PKG_DIR=`pkg_dir`
PROG_DIR=`dirname $0`
KOSHU_DIR=$PROG_DIR/koshucode-master/baala/calculator

if [ $# != 0 ]; then
    help
elif [ -d "$PKG_DIR" ]; then
    ( cabal_repl "$PKG_DIR" )
elif [ -d "$KOSHU_DIR" ]; then
    ( cabal_repl "$KOSHU_DIR" )
else
    help
fi
