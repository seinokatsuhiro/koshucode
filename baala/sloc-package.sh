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

all=no
update=no

case "$1" in
    -a) all=yes ;;
    -u) update=yes ;;
    -h) usage
        exit 2 ;;
esac

count () {
    echo "Count sloc for $1"
    ./sloc-haskell.pl `list $1` | post "$1/data/SLOC.k"
}

post () {
    if [ $all = yes ]; then
        cat
    elif [ $update = yes ]; then
        tee "$1" > /dev/null
    else
        grep TOTAL
    fi
}

list () {
    find "$1" -name '[A-Z]*.hs' | grep -v _ | grep -v Setup.hs
}

main () {
    for pkg in base core writer operator content calculator toolkit; do
        count $pkg
    done

    koshu sloc-all-list.k */data/SLOC.k > SLOC-ALL.k
}

main

