#!/bin/sh

io_version () {
    echo "koshu-inout-0.52"
    exit
}

io_usage () {
    echo "DESCRIPTION"
    echo "  Generate I/O list for koshu scripts."
    echo
    echo "USAGE"
    echo "  $io_cmd [OPTION ...] SCRIPT.k ..."
    echo
    echo "OPTION"
    echo "  -d          show differences from last document"
    echo "  -f FILE     take input files from FILE"
    echo "  -g          glob input files by *.$io_glob_ext"
    echo "  -h          print help message"
    echo "  -o FILE.md  save document to FILE.md"
    echo "  -p PROG     use PROGram instead of $io_program"
    echo "  -r          save document to README.md"
    echo "  -s          save document to INOUT.md"
    echo "  -t          do not delete temporary files"
    echo "  -x EXT      use EXTension instead of *.$io_glob_ext"
    echo
    echo "EXAMPLE"
    echo "  $io_cmd -r -g CALC.k"
    echo "  $io_cmd -o CALC.md -f FILE CALC.k"
    echo "  $io_cmd CALC.k DATA.k"
    echo
    exit
}

stderr () {
    if [ $io_error = output ]; then
        echo "$*" 1>&2
    fi
}

io_create_temporary () {
    mktemp TEMP-KOSHU-XXXX
}

io_delete_temporary () {
    if [ $io_keep_temp = no ]; then
        [ -f $1 ] && rm $1
    fi
}


# ============================================  Document

io_doc () {
    if [ -z "$io_output_work" ]; then
        io_doc_body $@
    else
        io_doc_body $@ > $io_output_work
        stderr "  Output to $io_output_work"
    fi
}

io_doc_body () {
    io_title
    io_body $@
    io_trailer
}

check_output () {
    case "$io_output_work" in
        *.$io_glob_ext )
              stderr "  Output file is probably input file."
              stderr "  Please check: $io_output_work"
              exit 2 ;; 
    esac
}

# ============================================  Table of contents

io_link       () { echo "(#$@)" | tr -d . | tr ' ' '-' \
                                | tr '[:upper:]' '[:lower:]'; }
io_table_calc () { echo "- [$@]`io_link $@`" ; }
io_table_data () { echo "- $io_program $io_calc [$@]`io_link "$@"`" ; }
io_table_from () { echo "$@" | xargs -n 1 | io_table ; }

io_table () {
    for k in $io_calc ; do io_table_calc $k ; done
    while read k      ; do io_table_data $k ; done
}


# ============================================  Heading

io_title () {
    echo "# I/O list of $io_program"
    echo
}

io_head () {
    echo
    echo
    echo
    echo "## $*"
    echo
}

io_linked_head () {
    io_head "[$1]($1)"
}

io_trailer () {
    io_head "command"

    echo "This document is produced by the command:"
    echo
    echo '```'
    echo "$io_cmdline"
    echo '```'
}


# ============================================  Input / Output

io_list () {
    echo '```'
    tr -d '\r' < "$1"
    echo '```'
}

io_list_all () {
    for k in $@; do
        io_list $k
    done
}

io_input () {
    if [ -f "$1" ]; then
        io_linked_head "$1"
        io_list "$1"
    fi
}

io_output () {
    $io_program "$@" > $io_temp
    io_status=$?
    stderr "  $io_status <- $io_program $@"

    echo
    if [ "$io_status" = 0 ]; then
        echo "Command \`$io_program $@\` produces:"
    else
        echo "Command \`$io_program $@\` exits with $io_status and produces:"
    fi

    echo
    io_list $io_temp
}


# ============================================  Contents

io_body () {
    case $io_glob_type in
        args) io_body_args $@ ;;
        file) io_body_file $@ ;;
        glob) io_body_glob $@ ;;
    esac
}

io_body_args () {
    # Table
    for k in $@ output; do io_table_calc $k; done
    # Input
    for k in $@; do io_input $k; done
    # Output
    io_head output
    io_output $@
}

io_body_file () {
    io_calc=$@

    # Table
    cat $io_glob_file | io_table
    # Calc
    for k in $io_calc; do io_input $k; done
    # Data
    cat $io_glob_file | while read io_data; do
        io_head $io_data
        io_list_all $io_data
        io_output $io_calc $io_data
    done
}

io_body_glob () {
    io_calc=$@
    io_data=`io_glob_data`

    # Table
    io_table_from $io_data
    # Calc
    for k in $io_calc; do io_input $k; done
    # Data
    for k in $io_data; do
        io_input $k
        io_output $io_calc $k
    done
}

io_glob_data () {(
    for k in *.$io_glob_ext; do
        if ! io_exist $k $io_calc; then
            echo $k
        fi
    done
)}

io_exist () {(
    target=$1
    shift

    for k in $@; do
        if [ $k = $target ]; then
            return 0
        fi
    done

    return 1
)}


# ============================================  Diff

io_diff () {
    if [ -f "$io_output_orig" ]; then
        io_output_work=`io_create_temporary`
        io_diff_body $@
        io_delete_temporary $io_output_work
    else
        echo "file not found: $io_output_orig"
        exit 2
    fi
}

io_diff_body () {
    io_doc $@  # output to $io_output_work

    if diff -u $io_output_orig $io_output_work > $io_temp; then
        io_diff_result OK
    else
        echo
        io_diff_result DIFF
        echo
        echo "Differences are found:"
        echo
        io_list $io_temp
        echo

        if [ $io_diff = 1 ]; then
            echo "To show all differences, please give more -d flag."
            echo "To examine this differences, type: cd `io_pwd`"
            echo
            io_status=2
            return
        fi
    fi
}

io_diff_result () {
    io_dir=`io_pwd`
    io_dir_md=`echo $io_dir | sed 's:/: / :g'`
    echo "- $1 – [$io_output_orig]($io_dir/$io_output_orig) in $io_dir_md"
}

io_pwd () {
    if [ -d "$IO_TOP" ]; then
        io_top=`echo $IO_TOP | tr '[:punct:]' .`'/*'
        pwd | sed "s:^$io_top::"
    else
        pwd
    fi
}

io_diff_del () {
    for arg in $*; do
        case $arg in
            -d)  ;;
            -dd) ;;
            *)   echo $arg ;;
        esac
    done | xargs
}


# ============================================  Main

# variable

io_cmd=`basename $0`
io_cmdline=`io_diff_del $io_cmd $*`
io_diff=0
io_error=output
io_glob_ext=k
io_glob_file=
io_glob_type=args
io_keep_temp=no
io_output_orig=
io_program=koshu
io_status=0

# option

while getopts df:gho:p:rstx:V opt; do
    case $opt in
        d)  case $io_diff in
              1) io_diff=2 ;;
              *) io_diff=1 ;;
            esac
            io_error=inhibit          ;;
        f)  io_glob_type=file
            io_glob_file=$OPTARG      ;;
        g)  io_glob_type=glob         ;;
        o)  io_output_orig=$OPTARG    ;;
        p)  io_program=$OPTARG        ;;
        r)  io_output_orig=README.md  ;;
        s)  io_output_orig=INOUT.md   ;;
        t)  io_keep_temp=yes          ;;
        x)  io_glob_ext=$OPTARG       ;;
        V)  io_version                ;;
        ?)  io_usage                  ;;
    esac
done

io_output_work=$io_output_orig
shift $(($OPTIND - 1))

# document

stderr "$io_cmdline"
check_output

io_temp=`io_create_temporary`

if [ $io_diff = 0 ]; then
    io_doc $@
else
    io_diff $@
fi

# clean up

io_delete_temporary $io_temp

exit $io_status
