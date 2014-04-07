#!/bin/sh

md_version () {
    echo "koshu-regress-0.50"
    exit
}

md_usage () {
    echo "DESCRIPTION"
    echo "  Do regression test using output of koshu-markdown.sh"
    echo
    echo "USAGE"
    echo "  $md_cmd [OPTION ...]"
    echo
    echo "OPTION"
    echo "  -d    show all differences"
    echo "  -h    print help message"
    echo "  -V    print version"
    echo
    echo "EXAMPLE"
    echo "  $md_cmd -d | tee REGRESS.k"
    echo
    exit
}

# ============================================  Diff

md_diff () {
    MD_TOP=`pwd`
    export MD_TOP

    if grep koshu-markdown.sh $sh > /dev/null; then
        cd `dirname $sh`
        ./`basename $sh` -d $md_more

        status=$?
        [ $status = 0 ] || exit $status
    fi
}

md_regress () {
    echo "** -*- koshu -*-"
    echo "**"
    echo "**  DESCRIPTION"
    echo "**    Result of comparing I/O lists."
    echo "**    This dataset is produced by $md_cmd."
    echo "**"
    echo

    for sh in `find . -name README.sh`; do
        ( md_diff )
    done
}

# ============================================  Main

md_cmd=`basename $0`
md_more=

while getopts dhV opt; do
    case $opt in
        d)  md_more=-d ;;
        V)  md_version ;;
        ?)  md_usage   ;;
    esac
done

md_regress

