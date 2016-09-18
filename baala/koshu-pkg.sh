#!/bin/sh

pkg_help () {
    echo "DESCRIPTION"
    echo "  Operations for each packages"
    echo
    echo "USAGE"
    echo "  $pkg_prog COMMAND"
    echo
    echo "COMMAND for listing"
    echo "  cabal            List cabal files"
    echo "  cabal-path       List paths of cabal files"
    echo "  copyright        List copyright in cabal files"
    echo "  dir              List package directory names"
    echo "  installed        List installed packages in sandbox"
    echo "  installed-koshu  List installed packages named 'koshu'"
    echo "  hoogle           List Hoogle files"
    echo "  hoogle P ...     Grep P ... for Hoogle files"
    echo "  synopsis         List synopses in cabal files"
    echo
    echo "COMMAND for executing"
    echo "  exec C           Execute C in each package directories"
    echo "  exec-all C       Same as exec except for ignoring exit status"
    echo "  init             Initilize sandbox"
    echo "  haddock          Generate Haddock documents"
    echo "  rehaddock        Regenerate Haddock documents"
    echo "  unreg            Unregister koshucode libraries"
    echo
}

pkg_section () {
    echo
    echo "***********  $*"
    echo
}

pkg_dirs () {
    echo subtext base syntax data core writer rop-flat rop-nested rop-cox cop calculator toolkit
}

pkg_dirs_rev () {
    echo toolkit calculator cop rop-cox rop-nested rop-flat writer core data syntax base subtext
}

pkg_cabal () {
    for pkg in `pkg_dirs`; do
        echo "koshucode-baala-$pkg.cabal"
    done
}

pkg_cabal_path () {
    for pkg in `pkg_dirs`; do
        pkg_cabal="$pkg_dir/$pkg/koshucode-baala-$pkg.cabal"
        if [ -e "$pkg_cabal" ]; then
            echo "$pkg_cabal"
        fi
    done
}

pkg_exec () {
    for pkg in `pkg_dirs`; do
        ( cd "$pkg"
          pkg_section "$* (in $pkg)"
          "$@"
        )
        pkg_status=$?
        if [ $pkg_status != 0 ]; then
            echo 
            echo "ABORTED (status $pkg_status)"
            exit 2
        fi
    done
}

pkg_exec_all () {
    for pkg in `pkg_dirs`; do
        ( cd "$pkg"
          pkg_section "$* (in $pkg)"
          "$@"
        )
        pkg_status=$?
        if [ $pkg_status != 0 ]; then
            echo "ABORTED (status $pkg_status)"
        fi
    done
}

pkg_haddock () {
    for pkg in `pkg_dirs`; do
        ( cd "$pkg"
          pkg_section "haddock (in $pkg)"
          cabal haddock \
              --hoogle \
              --hyperlink-source \
              --html-location=$pkg_doc_hackage \
              --haddock-option=--pretty-html \
              `pkg_haddock_option $pkg`
        )
    done
}

pkg_haddock_option () {
    for pkg in `pkg_dirs`; do
        if [ $pkg = $1 ]; then
            break
        fi
        pkg_name="koshucode-baala-$pkg"
        pkg_interface="../$pkg/dist/doc/html/$pkg_name/$pkg_name.haddock"
        if [ -e "$pkg_interface" ]; then
            echo "--haddock-option=--read-interface=$pkg_doc_koshu/$pkg_name,$pkg_interface"
        fi
    done
}

pkg_hoogle () {
    for pkg in `pkg_dirs`; do
        pkg_name="koshucode-baala-$pkg"
        pkg_hoogle=$pkg/dist/doc/html/$pkg_name/$pkg_name.txt
        if [ -e $pkg_hoogle ]; then
            echo $pkg_hoogle
        fi
    done
}

pkg_hoogle_grep () {
    for pkg in `pkg_hoogle`; do
        pkg_section "$pkg"
        grep -v "^--" $pkg | pkg_grep_$# "$@" | sort | cat -n
    done
}

g () { grep -i "$1" ; }

pkg_grep_1 () { g "$1" ; }
pkg_grep_2 () { g "$1" | g "$2" ; }
pkg_grep_3 () { g "$1" | g "$2" | g "$3" ; }
pkg_grep_4 () { g "$1" | g "$2" | g "$3" | g "$4" ; }
pkg_grep_5 () { g "$1" | g "$2" | g "$3" | g "$4" | g "$5" ; }

pkg_sandbox_init () {
    if [ -e cabal.sandbox.config ]; then
        echo "Sandbox is already initialized."
    else
        cabal sandbox init --sandbox ../cabal/sandbox
    fi
}

pkg_sandbox_installed () {
    if [ -e "$pkg_dir/toolkit/cabal.sandbox.config" ]; then
        ( cd "$pkg_dir/toolkit" ;
          cabal exec ghc-pkg -- list "$1" --simple-output | xargs -n 1
        )
    fi
}

pkg_cabal_section () {
    for pkg_cabal in `pkg_cabal_path`; do
        pkg_base=`basename "$pkg_cabal"`
        pkg_label=`printf "%-40s " "$pkg_base"`
        sed -n "s/^$1: */$pkg_label/p" "$pkg_cabal"
    done
}

pkg_unreg () {
    if [ -e cabal.sandbox.config ]; then
        for pkg in `pkg_dirs_rev`; do
            cabal exec ghc-pkg unregister koshucode-baala-$pkg
        done
    else
        echo "No sandbox config"
    fi

}

# --------------------------------------------  main

pkg_prog=koshu-pkg.sh
pkg_dir=`pwd | sed -n 's:\(.*/koshucode-master/baala\).*:\1:p'`

pkg_doc_koshu=http://seinokatsuhiro.github.io/koshucode/doc/html
pkg_doc_hackage='http://hackage.haskell.org/package/$pkg/docs'


case "$1" in
    cabal)
        pkg_cabal ;;
    cabal-path)
        pkg_cabal_path ;;
    copyright)
        pkg_cabal_section copyright ;;
    dir)
        pkg_dirs | xargs -n 1 ;;
    dir-rev)
        pkg_dirs_rev | xargs -n 1 ;;
    exec)
        shift
        pkg_exec "$@" ;;
    exec-all)
        shift
        pkg_exec_all "$@" ;;
    init)
        pkg_exec pkg_sandbox_init ;;
    installed)
        pkg_sandbox_installed "*" ;;
    installed-koshu)
        pkg_sandbox_installed "koshu*" ;;
    haddock)
        pkg_haddock ;;
    hoogle)
        shift
        if [ $# = 0 ]; then
            pkg_hoogle
        else
            pkg_hoogle_grep "$@"
        fi ;;
    rehaddock)
        pkg_exec cabal configure
        pkg_exec cabal build
        pkg_haddock ;;
    synopsis)
        pkg_cabal_section synopsis ;;
    unreg)
        pkg_unreg ;;
    *)
        pkg_help ;;
esac

