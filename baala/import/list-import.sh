#!/bin/sh

usage () {
    echo "USAGE"
    echo "  $0 [-au]"
    echo
    echo "DESCRIPTION"
    echo "  List module imports in each packages."
    echo
    echo "OPTOINS"
    echo "  -a  Show all"
    echo "  -u  Update $imp_file"
    echo
}

imp_all=no
imp_update=no
imp_file=IMPORT.k

case "$1" in
    -a) imp_all=yes ;;
    -u) imp_update=yes ;;
    -h) usage
        exit 2 ;;
esac

imp_body () {
    echo "Imports for $1"
    ./list-import.pl `imp_list $1` | imp_post "$1/data/$imp_file"
}

imp_list () {
    find "$1" -name '[A-Z]*.hs' | grep -v _ | grep -v Setup.hs
}

imp_post () {
    if [ $imp_all = yes ]; then
        cat
    elif [ $imp_update = yes ]; then
        tee "$1" > /dev/null
    else
        grep IMPORT
    fi
}

imp_main () {
    for pkg in base data core writer operator content calculator toolkit; do
        imp_body ../$pkg
    done
}

imp_main

