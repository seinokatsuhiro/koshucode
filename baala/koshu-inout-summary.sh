#!/bin/sh

io_version () {
    echo "koshu-inout-summary-0.55"
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
    echo "  -f FILE     find FILE and invoke it for each I/O list"
    echo "  -g          grand summary mode"
    echo "  -h          print help message"
    echo "  -i          generate INOUT.sh template"
    echo "  -l          link to each reports"
    echo "  -o FILE     save report to FILE"
    echo "  -s          save report to INOUT-[GRAND]-SUMMARY.md"
    echo "  -u          update I/O list interactively"
    echo "  -V          print version"
    echo
    echo "FILENAME"
    echo "  INOUT.sh                 script that outputs I/O list"
    echo "  INOUT.md                 I/O list"
    echo "  INOUT-SUMMARY.md         summary report of I/O lists"
    echo "  INOUT-GRAND-SUMMARY.md   summary of summaries"
    echo
    echo "  This command collects each I/O lists into summary report,"
    echo "  when -g mode, collects summaries into grand summary."
    echo
    exit
}


# ============================================  Path

io_path_split () {
    io_base=`basename $1`
    io_dir=`dirname $1`
}

io_path_item () {
    io_path_split $1
    io_dir_spaced=`echo $io_dir | sed "s:/: / :g"`

    if [ "$io_link" = -l ]; then
        echo "* [$io_base]($io_dir/$io_base) in $io_dir_spaced"
    else
        echo "* $io_base in $io_dir_spaced"
    fi
}


# ============================================  Summary

io_summary () {
    IO_TOP=`pwd`
    export IO_TOP

    echo "# Summary of I/O Lists"
    echo
    echo "This is a summary of I/O lists."
    echo "Each entry consists of a result of regression test,"
    echo "a markdown file, and its directory."
    echo "Results of regression tests are recorded in OK or DIFF."
    echo "This summary is produced by the command \`$io_cmd\`."
    echo

    for sh in `find . -name $io_find`; do
        io_diff || exit $?
    done
}

io_diff () {
    if grep koshu-inout.sh $sh > /dev/null; then
        (   io_path_split $sh
            cd $io_dir
            ./$io_base -d $io_link $io_more
        )   || exit $status
    fi
}


# ============================================  Grand summary

io_grand () {
    echo "# Grand Summary of I/O Lists"
    echo
    echo "This is a grand summary of I/O lists."
    echo "This summary is produced by the command \`$io_cmd\`."
    echo

    for md in `find . -name $io_find | sed "s:^[.]/::"`; do
        [ $md != $io_find ] && io_path_item $md
    done
}


# ============================================  Generate INOUT.sh

io_gen () {
    if [ -f $io_find ]; then
        echo "$io_find already exists."
        exit 2
    fi

    io_gen_content > $io_find

    if [ $? = 0 ]; then
        echo "$io_find is generated, please edit the contents."
        chmod 755 $io_find
    else
        echo "$io_find is not generated."
        exit 2
    fi
}

io_gen_content () {
    echo "#!/bin/sh"
    echo "koshu-inout.sh \$* -s -g koshu"
}


# ============================================  Command line

io_decide_file () {
    if [ -z $io_find ]; then
        case $io_proc in
            io_summary ) io_find=INOUT.sh ;;
            io_gen     ) io_find=INOUT.sh ;;
            io_grand   ) io_find=INOUT-SUMMARY.md ;;
        esac
    fi

    if [ -z "$io_output" -a $io_output_yn = y ]; then
        case $io_proc in
            io_summary ) io_output=INOUT-SUMMARY.md ;;
            io_grand   ) io_output=INOUT-GRAND-SUMMARY.md ;;
        esac
    fi
}

io_cmd=`basename $0`
io_find=
io_link=
io_more=
io_output=
io_output_yn=n
io_proc=io_summary

while getopts df:ghilo:suV io_opt; do
    case $io_opt in
        d)  io_more=-d　        ;;
        f)  io_find=$OPTARG     ;;
        g)  io_proc=io_grand    ;;
        i)  io_proc=io_gen      ;;
        l)  io_link=-l          ;;
        o)  io_output=$OPTARG
            io_output_yn=y      ;;
        s)  io_output_yn=y      ;;
        u)  io_more=-u          ;;
        V)  io_version          ;;
        ?)  io_usage            ;;
    esac
done

io_decide_file


# ============================================  Main

if [ ! -z "$io_output" -a $io_output_yn = y ]; then
    $io_proc | tee $io_output
    echo
    echo "(Copy of this report is saved to $io_output)"
else
    $io_proc
    echo
fi

