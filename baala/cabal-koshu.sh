#!/bin/sh
# ------------------------------------------------------------------
#
#  DESCRIPTION
#    Cabal for each packages
#
#  USAGE
#    [1] ./cabal-koshu.sh link
#          Make symbolic links 'install.link', etc.
#    [2] ./install.link base
#          Build and install base package.
#    [3] ./install.link
#          Build and install all packages.
#
# ------------------------------------------------------------------

usage () {
    echo "cabal for each packages"
    echo ""

    if [ -z $command ]; then
        echo "  $0 clean          cleaning directories"
        echo "  $0 link           make symbolic-linked commands"
        echo "  $0 unreg          unregister koshucode packages"
        echo "  $0 update         update cabal packages"
        echo ""
    else
        echo "  $0                cabal for all packages"
        echo "  $0 koshu          cabal to install koshu program"
        echo ""
        echo "  $0 base           cabal for base package"
        echo "  $0 core           cabal for core package"
        echo "  $0 operator       cabal for operator package"
        echo "  $0 calculator     cabal for calculator package"
        echo "  $0 toolkit        cabal for toolkit package"
        echo ""
    fi
}

main () {
    decide_command `basename $0`

    case "$1" in
        clean)
            command=cabal_clean
            cabal_for base core operator calculator toolkit
            exit ;;

        link)
            # alphabetical order
            sym_link "$0" haddock.link
            sym_link "$0" html.link
            sym_link "$0" install.link
            sym_link "$0" sdist.link
            sym_link "$0" sloc.link
            exit ;;

        unreg)
            unregister toolkit
            unregister calculator
            unregister operator
            unregister core
            unregister base
            exit ;;

        update)
            cabal_cmd update
            exit ;;

        '' | base* | core* | operator* | calculator* | toolkit* | koshu )
            cabal_for `directories "$1"`
            exit ;;

        *)
            usage
            exit ;;
    esac
}

decide_command () {
    case "$1" in
        haddock.link)
            command=cabal_haddock
            echo "haddock -- generate Haddock HTML documentation" ;;
        html.link)
            command=cabal_html
            echo "html -- open html documents" ;;
        install.link)
            command=cabal_install
            echo "install -- installs packages" ;;
        sdist.link)
            command=cabal_sdist
            echo "sdist -- generate a source distribution file" ;;
        sloc.link)
            command=sloc
            echo "sloc -- count source lines of code" ;;
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

unregister () {
    cabal_cmd ghc-pkg unregister koshucode-baala-$1
}

directories () {
    case `basename "$1"` in
        base)        echo base ;;
        core)        echo core ;;
        operator)    echo operator ;;
        calculator)  echo calculator ;;
        toolkit)     echo toolkit ;;
        koshu)       echo base core operator calculator ;;
        '')          echo base core operator calculator toolkit ;;
    esac
}



# ======================  cabal

CABAL_DEV=$HOME/cabal-dev
GITHUB_DOC=http://seinokatsuhiro.github.io/koshucode/doc/html
URL=http://hackage.haskell.org/packages/archive/base/latest/doc/html

cabal_cmd () {
    echo "# $@"
    cabal-dev --sandbox=$CABAL_DEV/koshucode "$@"
    #cabal "$@"
}

cabal_for () {
    if [ -z $command ]; then
        usage
        exit
    fi

    for d in "$@"; do
        echo
        echo "================================= $d"
        (cd "$d"; time $command "$d") || exit 1
    done
}

section () {
    echo "----------------------  $*"
}

cabal_clean () {
    cabal_cmd clean
}



# ======================  commands for each packages

cabal_haddock () {
    if [ `pwd_base` = XXXcalculator ]; then
        cabal_cmd haddock --executables
    else
        cabal_cmd haddock
    fi
}

pwd_base () {
    basename $PWD
}

cabal_html () {
    open dist/doc/html/koshucode-baala-*/index.html
}

cabal_install () {
    cabal_cmd install \
        --force-reinstalls \
        --disable-documentation \
        --disable-optimization
}

cabal_sdist () {
    cabal_cmd clean
    cabal_cmd configure

    section hoogle
    cabal_cmd haddock --hoogle

    section haddock
    cabal_cmd haddock \
        --hyperlink-source \
        --hscolour-css=../hscolour.css \
        --haddock-option=--pretty-html \
        --haddock-option=`if_file base` \
        --haddock-option=`if_file core` \
        --haddock-option=`if_file operator` \
        --haddock-option=`if_file calculator` \
        --html-location=$URL
        # --executable \

    section sdist
    cabal_cmd sdist
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

sloc () {
    ../sloc-haskell.pl `find . -name "[A-Z]*.hs"`
}

main "$1"

