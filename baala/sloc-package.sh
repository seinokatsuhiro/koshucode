#!/bin/sh

usage () {
    echo "USAGE"
    echo "  $0 [-au]"
    echo
    echo "DESCRIPTION"
    echo "  Counting source lines of code in each packages."
    echo "  It shows summary without options."
    echo
    echo "OPTOINS"
    echo "  -a  Show all files"
    echo "  -u  Update SLOC.k"
    echo
}

sloc_count () {
    echo "Count sloc for $1"
    ./sloc-haskell.pl `sloc_list $1` | sloc_post "$1/data/SLOC.k"
}

sloc_post () {
    if [ $sloc_all = yes ]; then
        cat
    elif [ $sloc_update = yes ]; then
        tee "$1" > /dev/null
    else
        grep TOTAL
    fi
}

sloc_list () {
    find "$1" -name '[A-Z]*.hs' | grep -v _ | grep -v Setup.hs
}

sloc_main () {
    for pkg in `koshu-pkg.sh dir`; do
        sloc_count $pkg
    done

    if [ $sloc_update = yes ]; then
        echo "Concat sloc files"
        koshu sloc-all-list.k */data/SLOC.k > SLOC-ALL.k
    fi
}

sloc_all=no
sloc_update=no

case "$1" in
    -a) sloc_all=yes ;;
    -u) sloc_update=yes ;;
    -h) usage
        exit 2 ;;
esac

sloc_main

