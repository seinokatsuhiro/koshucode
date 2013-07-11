#!/bin/sh
#
#  Cabal for each directories
#
#  [1] ./cabal-koshu.sh link
#        Make symbolic links 'install.link', etc.
#  [2] ./install.link base
#        Build and install base package.
#  [3] ./install.link
#        Build and install all packages.
#

usage () {
    echo "cabal for each directories"
    echo ""
    if [ ! -z $cabal ]; then
    echo "  $0                cabal for all directories"
    echo ""
    echo "  $0 base           cabal for base directory"
    echo "  $0 operator       cabal for operator directory"
    echo "  $0 calculator     cabal for calculator directory"
    echo "  $0 toolkit        cabal for toolkit directory"
    echo ""
    fi
    echo "  $0 link           make symbolic-linked commands"
    echo "  $0 unreg          unregister koshucode packages"
    echo ""
}

main () {
    decide_program `basename $0`

    case "$1" in
        link)
            sym_link "$0" sdist.link
            sym_link "$0" haddock.link
            sym_link "$0" install.link
            exit ;;

        unreg)
            unregister toolkit
            unregister calculator
            unregister operator
            unregister base
            exit ;;

        '' | base* | operator* | calculator* | toolkit*)
            cabal_for `directories "$1"`
            exit ;;

        *)
            usage
            exit ;;
    esac
}

unregister () {
    echo "Unregistering $1"
    ghc-pkg unregister koshucode-baala-$1
}

decide_program () {
    case "$1" in
        sdist.link)
            cabal=cabal_sdist
            echo "sdist ** generate a source distribution file" ;;
        haddock.link)
            cabal=cabal_haddock
            echo "haddock ** generate Haddock HTML documentation" ;;
        install.link)
            cabal=cabal_install
            echo "install ** installs packages" ;;
    esac
}

directories () {
    case `basename "$1"` in
        base)        echo base ;;
        operator)    echo operator ;;
        calculator)  echo calculator ;;
        toolkit)     echo toolkit ;;
        '')          echo base operator calculator toolkit ;;
    esac
}

sym_link () {
    if [ -e "$2" ]; then
        echo "already exists: $2"
    else
        echo "making $2"
        ln -s "$1" "$2"
    fi
}

# ======================  cabal command

URL=http://hackage.haskell.org/packages/archive/base/latest/doc/html
GITHUB_DOC=http://seinokatsuhiro.github.io/koshucode/doc/html

cabal_for () {
    if [ -z $cabal ]; then
        usage
        exit
    fi

    for d in "$@"; do
        echo
        echo "================================= $d"
        (cd "$d"; time $cabal "$d") || exit 1
    done
}

section () {
    echo "----------------------  $*"
}

cabal_sdist () {
    cabal clean
    cabal configure

    section hoogle
    cabal haddock --hoogle

    section haddock
    cabal haddock \
        --hyperlink-source \
        --hscolour-css=../hscolour.css \
        --haddock-option=--pretty-html \
        --haddock-option=`if_file base` \
        --haddock-option=`if_file operator` \
        --haddock-option=`if_file calculator` \
        --html-location=$URL
        # --executable \

    section sdist
    cabal sdist
}

# interface file
if_file () {
    package=koshucode-baala-$1
    haddock=../$1/dist/doc/html/$package/$package.haddock
    if [ `pwd_base` = $1 ]; then
        echo --html
    elif [ -e $haddock ]; then
        echo --read-interface=$GITHUB_DOC/$package/,$haddock
    else
        echo --html
    fi
}

cabal_haddock () {
    if [ `pwd_base` = XXXcalculator ]; then
        cabal haddock --executables
    else
        cabal haddock
    fi
}

pwd_base () {
    basename $PWD
}

cabal_install () {
    cabal install \
        --force-reinstalls \
        --disable-documentation \
        --disable-optimization
}

main "$1"

