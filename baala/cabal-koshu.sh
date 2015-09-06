#!/bin/sh
# ------------------------------------------------------------------
#
#  DESCRIPTION
#    Cabal for each packages
#
#  USAGE
#    [1] ./cabal-koshu.sh sandbox-init
#          Initialize cabal/sandbox
#
#    [2] ./cabal-koshu.sh sandbox-deps
#          Install dependent libraries
#
#    [3] ./cabal-koshu.sh link
#          Make symbolic links 'cabal/build.link', etc.
#
#    [4] cabal/build.link base
#          Build base package.
#
#    [5] cabal/build.link
#          Build all packages.
#
# ------------------------------------------------------------------

usage () {
    echo "cabal for each packages"
    echo ""

    if [ -z $cab_command ]; then
        echo "  $0 clean             cleaning directories"
        echo "  $0 link              make symbolic-linked commands"
        echo "  $0 ls                list files"
        echo "  $0 sandbox-delete    delete cabal sandbox"
        echo "  $0 sandbox-deps      install dependent libraries"
        echo "  $0 sandbox-init      initialize cabal sandbox"
        echo "  $0 unreg             unregister koshucode packages"
        echo "  $0 update            update cabal packages"
        echo ""
    else
        echo "  $0                   cabal for all packages"
        echo "  $0 koshu             cabal to install koshu program"
        echo ""
        echo "  $0 base              cabal for base package"
        echo "  $0 core              cabal for core package"
        echo "  $0 writer            cabal for writer package"
        echo "  $0 operator          cabal for operator package"
        echo "  $0 content           cabal for content package"
        echo "  $0 calculator        cabal for calculator package"
        echo "  $0 toolkit           cabal for toolkit package"
        echo ""
    fi
}

main () {
    cab_time=non
    decide_command `basename $0`

    case "$1" in
        clean)
            cabal_for_all cabal_clean ;;

        link)
            # alphabetical order
            ( cd cabal
              sym_link "$0" build.link
              sym_link "$0" haddock.link
              sym_link "$0" html.link
              sym_link "$0" install.link
              sym_link "$0" sdist.link
              sym_link "$0" sloc.link
            ) ;;

        ls)
            cabal_for_all cabal_ls ;;

        sandbox-init)
            cabal_for_all cabal_sandbox_init ;;

        sandbox-deps)
            cabal_for_all cabal_sandbox_deps ;;

        sandbox-delete)
            cabal_for_all cabal_sandbox_delete
            [ -d cabal/sandbox ] && rm -r cabal/sandbox
            ;;

        unreg)
            unregister toolkit
            unregister calculator
            unregister content
            unregister operator
            unregister writer
            unregister core
            unregister base
            ;;

        update)
            cabal_cmd update ;;

        '' | base* | core* | writer* | operator* | content* | calculator* | toolkit* | koshu )
            cab_time=time
            cabal_for `directories "$1"`
            ;;

        *)
            usage ;;
    esac
}

decide_command () {
    case "$1" in
        build.link)
            cab_command=cabal_build
            echo "build -- build packages" ;;
        haddock.link)
            cab_command=cabal_haddock
            echo "haddock -- generate Haddock HTML documentation" ;;
        html.link)
            cab_command=cabal_html
            echo "html -- open html documents" ;;
        install.link)
            cab_command=cabal_install
            echo "Using" `ghc -V`
            echo
            echo "install -- installs packages" ;;
        sdist.link)
            cab_command=cabal_sdist
            echo "sdist -- generate a source distribution file" ;;
        sloc.link)
            cab_command=sloc
            echo "sloc -- count source lines of code" ;;
    esac
}

sym_link () {
    if [ -e "$2" ]; then
        echo "already exists: cabal/$2"
    else
        echo "making cabal/$2"
        ln -s "../$1" "$2"
    fi
}

unregister () {(
    cd $1
    cabal_cmd sandbox hc-pkg unregister koshucode-baala-$1
)}

directories () {
    case `basename "$1"` in
        base)        echo base ;;
        core)        echo core ;;
        writer)      echo writer ;;
        operator)    echo operator ;;
        content)     echo content ;;
        calculator)  echo calculator ;;
        toolkit)     echo toolkit ;;
        koshu)       echo base core writer operator content calculator ;;
        '')          echo base core writer operator content calculator toolkit ;;
    esac
}



# ======================  cabal

CAB_GITHUB_DOC=http://seinokatsuhiro.github.io/koshucode/doc/html
CAB_URL=http://hackage.haskell.org/packages/archive/base/latest/doc/html

cabal_cmd () {
    echo "# $@"
    cabal "$@"
}

cabal_for () {
    if [ -z $cab_command ]; then
        usage
        exit
    fi

    for cab_dir in "$@"; do
        echo
        echo "================================= $cab_dir"

        ( cd "$cab_dir"
          if [ $cab_time = time ]; then
              time $cab_command "$cab_dir" 
          else
              $cab_command "$cab_dir" 
          fi
        ) || exit 1
    done
}

cabal_for_all () {
    cab_command=$1
    cabal_for base core writer operator content calculator toolkit
}

section () {
    echo "----------------------  $*"
}

cabal_clean () {
    cabal_cmd clean
}

cabal_ls () {
    ls -l
}


# ======================  sandbox commands

cabal_sandbox_init () {
    if [ ! -f cabal.sandbox.config ]; then
        cabal_cmd sandbox init --sandbox=../cabal/sandbox
    fi
}

cabal_sandbox_delete () {
    if [ -f cabal.sandbox.config ]; then
        cabal_cmd sandbox delete
        echo "(exit with $?)"
    else
        echo "not in sandbox"
    fi
}

cabal_sandbox_deps () {
    if [ -f cabal.sandbox.config ]; then
        cabal_sandbox_add_source
        cabal_cmd install --only-dependencies
    fi
}

cabal_sandbox_add_source () {
    case $cab_dir in
        core)        cabal_cmd sandbox add-source ../base       ;;
        writer)      cabal_cmd sandbox add-source ../base       ;;
        operator)    cabal_cmd sandbox add-source ../core       ;;
        content)     cabal_cmd sandbox add-source ../operator
                     cabal_cmd sandbox add-source ../writer     ;;
        calculator)  cabal_cmd sandbox add-source ../content    ;;
        toolkit)     cabal_cmd sandbox add-source ../calculator ;;
    esac
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

cabal_build () {
    cabal_cmd build
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
        --haddock-option=`if_file writer` \
        --haddock-option=`if_file operator` \
        --haddock-option=`if_file content` \
        --haddock-option=`if_file calculator` \
        --html-location=$CAB_URL
        # --executable \

    section sdist
    cabal_cmd sdist
}

# interface file
if_file () {
    cab_package=koshucode-baala-$1
    cab_haddock=../$1/dist/doc/html/$cab_package/$cab_package.haddock
    if [ `pwd_base` = $1 ]; then
        echo --html
    elif [ -e $cab_haddock ]; then
        echo --read-interface=$CAB_GITHUB_DOC/$cab_package/,$cab_haddock
    else
        echo --html
    fi
}

sloc () {
    ../sloc-haskell.pl `find . -name "[A-Z]*.hs"`
}

main "$1"

