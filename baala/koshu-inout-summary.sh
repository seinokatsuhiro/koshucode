#!/bin/sh

io_version () {
    echo "koshu-inout-summary-0.51"
    exit
}

io_usage () {
    echo "DESCRIPTION"
    echo "  Output summary report on I/O lists in subdirectories"
    echo
    echo "USAGE"
    echo "  $io_cmd [OPTION ...]"
    echo
    echo "OPTION"
    echo "  -d          show all differences"
    echo "  -f FILE.sh  find FILE.sh and invoke it for each I/O list"
    echo "  -h          print help message"
    echo "  -r          save report to README.md"
    echo "  -s          save report to INOUT-SUMMARY.md"
    echo "  -V          print version"
    echo
    echo "EXAMPLE"
    echo "  $io_cmd -d | tee INOUT-SUMMARY.md"
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

io_summary () {
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

    for sh in `find . -name $io_script`; do
        ( io_diff ) || exit $?
    done
}

# ============================================  Main

io_cmd=`basename $0`
io_more=
io_output=
io_script=INOUT.sh

while getopts df:hrsV opt; do
    case $opt in
        d)  io_more=-d          ;;
        f)  io_script=$OPTARG   ;;
        r)  io_output=README.md ;;
        s)  io_output=INOUT-SUMMARY.md ;;
        V)  io_version          ;;
        ?)  io_usage            ;;
    esac
done

if [ -z "$io_output" ]; then
    io_summary
else
    io_summary | tee $io_output
    echo
    echo "(Copy of this report is saved to $io_output)"
fi

