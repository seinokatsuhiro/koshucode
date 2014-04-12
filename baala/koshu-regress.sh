#!/bin/sh

io_version () {
    echo "koshu-regress-0.51"
    exit
}

io_usage () {
    echo "DESCRIPTION"
    echo "  Do regression test using output of koshu-markdown.sh"
    echo
    echo "USAGE"
    echo "  $io_cmd [OPTION ...]"
    echo
    echo "OPTION"
    echo "  -d    show all differences"
    echo "  -h    print help message"
    echo "  -V    print version"
    echo
    echo "EXAMPLE"
    echo "  $io_cmd -d | tee REGRESS.k"
    echo
    exit
}

# ============================================  Diff

io_diff () {
    if grep koshu-inout.sh $sh > /dev/null; then
        cd `dirname $sh`
        ( ./`basename $sh` -d $io_more ) || exit $status
    fi
}

io_regress () {
    IO_TOP=`pwd`
    export IO_TOP

    echo "# Report on I/O lists"
    echo
    echo "This is a summary of I/O lists."
    echo "Each entry consists of a result of regression test,"
    echo "a markdown file, and its directory."
    echo "Results of regression tests are recorded in OK or DIFF."
    echo "This summary is produced by the command \`$io_cmd\`"
    echo

    for sh in `find . -name $io_shell`; do
        ( io_diff ) || exit $?
    done
}

# ============================================  Main

io_cmd=`basename $0`
io_more=
io_shell=INOUT.sh

while getopts dhs:V opt; do
    case $opt in
        d)  io_more=-d       ;;
        V)  io_version       ;;
        s)  io_shell=$OPTARG ;;
        ?)  io_usage         ;;
    esac
done

io_regress

