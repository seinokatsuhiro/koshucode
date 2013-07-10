#!/bin/sh
#
#  DESCRIPTION
#    Generate CALC.k
#
#  USAGE
#    ./CALC.sh > CALC.k
#

comment () {
    cat <<EOF
** -*- koshu -*-
**
**  DESCRIPTION
**    Calculation list of dictionary examples.
**
**  USAGE
**    koshu --calc CALC.k
**

EOF
}

koshu_calc () {
    printf "|-- KOSHU-CALC  /input %-35s /output %s\n" "$1" "$2"
}

koshu_calc_list () {
    for k in [a-z]*.k; do
        koshu_calc "[ DATA.k $k ]" "output-$k\n"
    done
    echo
}

comment
koshu_calc_list

