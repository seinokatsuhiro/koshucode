#!/bin/sh

pkg_help () {
    echo "USAGE"
    echo "  $pkg_prog COMMAND"
    echo
    echo "COMMAND"
    echo "  cabal        List cabal files"
    echo "  cabal-path   List paths of cabal files"
    echo "  copyright    List copyright in cabal files"
    echo "  dir          List package directory names"
    echo "  sandbox      List installed packages in sandbox"
    echo "  synopsis     List synopses in cabal files"
    echo
}

pkg_names () {
    echo base data core writer rop-flat rop-nested cop content calculator toolkit
}

pkg_cabal () {
    for pkg in `pkg_names`; do
        echo "koshucode-baala-$pkg.cabal"
    done
}

pkg_cabal_path () {
    for pkg in `pkg_names`; do
        pkg_cabal="$pkg_dir/$pkg/koshucode-baala-$pkg.cabal"
        if [ -e "$pkg_cabal" ]; then
            echo "$pkg_cabal"
        fi
    done
}

pkg_dir () {
    pkg_names | xargs -n 1
}

pkg_sandbox () {
    if [ -e "$pkg_dir/toolkit/cabal.sandbox.config" ]; then
        ( cd "$pkg_dir/toolkit" ;
          cabal exec ghc-pkg -- list 'koshu*' --simple-output | xargs -n 1
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

# --------------------------------------------  main

pkg_prog=koshu-pkg.sh
pkg_dir=`pwd | sed -n 's:\(.*/koshucode-master/baala\).*:\1:p'`

case "$1" in
    cabal)
        pkg_cabal ;;
    cabal-path)
        pkg_cabal_path ;;
    copyright)
        pkg_cabal_section copyright ;;
    dir)
        pkg_dir ;;
    sandbox)
        pkg_sandbox ;;
    synopsis)
        pkg_cabal_section synopsis ;;
    *)
        pkg_help ;;
esac

