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

cabal_repl () {(
    echo "-- Repl in $1"
    cd "$1"
    cabal repl
)}

koshu_ghci () {
    if [ -f cabal.sandbox.config ]; then
        cabal_repl `pwd`
    elif [ -d "$PKG_DIR" ]; then
        cabal_repl "$PKG_DIR"
    elif [ -d "$KOSHU_DIR" ]; then
        cabal_repl "$KOSHU_DIR"
    else
        help
    fi
}

koshu_ghc () {
    if [ -d "$KOSHU_PKG_DB" ]; then
        echo "-- Package DB: $KOSHU_PKG_DB"
        ghc -rtsopts -package-db "$KOSHU_PKG_DB" "$@"
    else
        help
    fi
}

PKG_DIR=`pkg_dir`
PROG_DIR=`dirname $0`
KOSHU_MASTER=$PROG_DIR/koshucode-master
KOSHU_DIR=$KOSHU_MASTER/baala/calculator
KOSHU_PKG_DB=`echo $KOSHU_MASTER/baala/cabal/sandbox/*-packages.conf.d`

if [ $# = 0 ]; then
    koshu_ghci
else
    koshu_ghc "$@"
fi
