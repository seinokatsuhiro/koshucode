#!/bin/sh

io_version () {
    echo "koshu-inout-summary-0.54"
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
    echo "  -c          print all markdown reports"
    echo "  -d          show all differences"
    echo "  -f FILE     find FILE ($io_script) and invoke it for each I/O list"
    echo "  -h          print help message"
    echo "  -l          link to each reports"
    echo "  -r          save report to README.md"
    echo "  -s          save report to INOUT-SUMMARY.md"
    echo "  -V          print version"
    echo
    echo "EXAMPLE"
    echo "  $io_cmd"
    echo "  $io_cmd -d | tee $io_script"
    echo
    exit
}


# ============================================  Report

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
        io_diff || exit $?
    done
}

io_diff () {
    if grep koshu-inout.sh $sh > /dev/null; then
        (   io_split_path $sh
            cd $io_dir
            ./$io_base -d $io_more
        )   || exit $status
    fi
}

io_split_path () {
    io_base=`basename $1`
    io_dir=`dirname $1`
}


# ============================================  Collection

io_collect () {
    echo "# Collection of reports"
    echo

    for md in `find . -name $io_output | sed "s:^[.]/::"`; do
        [ $md != $io_output ] && io_path_item $md
    done
}

io_path_item () {
    io_split_path $1
    io_dir_spaced=`echo $io_dir | sed "s:/: / :g"`

    if [ $io_link_yn = y ]; then
        echo "* [$io_base]($io_dir/$io_base) in $io_dir_spaced"
    else
        echo "* $io_base in $io_dir_spaced"
    fi
}


# ============================================  Main

io_cmd=`basename $0`
io_link_yn=n
io_more=
io_output=INOUT-SUMMARY.md
io_output_yn=no
io_proc=io_summary
io_script=INOUT.sh

while getopts cdf:hlrsV io_opt; do
    case $io_opt in
        c)  io_proc=io_collect  ;;
        d)  io_more=-dd         ;;
        f)  io_script=$OPTARG   ;;
        l)  io_link_yn=y        ;;
        r)  io_output_yn=y
            io_output=README.md ;;
        s)  io_output_yn=y ;;
        V)  io_version          ;;
        ?)  io_usage            ;;
    esac
done

if [ $io_output_yn = y ]; then
    $io_proc | tee $io_output
    echo
    echo "(Copy of this report is saved to $io_output)"
else
    $io_proc
    echo
fi

