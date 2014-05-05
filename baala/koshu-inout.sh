#!/bin/sh

io_version () {
    echo "koshu-inout-0.54"
    exit
}

io_usage () {
    echo "DESCRIPTION"
    echo "  Generate I/O list"
    echo
    echo "USAGE"
    echo "  $io_cmd [OPTION ...] koshu FILE.k ..."
    echo
    echo "OPTION"
    echo "  -d          show differences from last document"
    echo "  -f FILE     take input files from FILE"
    echo "  -g          glob input files by *.$io_glob_ext"
    echo "  -h          print help message"
    echo "  -l          link to I/O list when -d specified"
    echo "  -o FILE.md  save document to FILE.md"
    echo "  -r          save document to README.md"
    echo "  -s          save document to INOUT.md"
    echo "  -t          do not delete temporary files"
    echo "  -x EXT      use EXTension instead of *.$io_glob_ext"
    echo
    echo "EXAMPLE"
    echo "  $io_cmd -r -g koshu CALC.k"
    echo "  $io_cmd -o CALC.md -f FILE koshu CALC.k"
    echo "  $io_cmd koshu CALC.k DATA.k"
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
    if [ $io_keep_temp_yn = n ]; then
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


# ============================================  Table of contents

io_table_calc () { echo "- [$@]`io_link $@`" ; }
io_table_data () { echo "- $io_calc [$@]`io_link "$@"`" ; }
io_table_from () { echo "$@" | xargs -n 1 | io_table ; }

io_link () {
    echo "(#$@)" \
        | tr -d ./ \
        | tr ' ' '-' \
        | tr '[:upper:]' '[:lower:]'
}

io_table () {
    for k in $io_calc ; do
        [ -f $k ] && io_table_calc $k
    done

    while read k; do
        io_table_data $k
    done
}


# ============================================  Heading

io_title () {
    echo "# I/O List"
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
    echo "$io_cmd_line"
    echo '```'
}


# ============================================  Input / Output

io_list () {
    [ -f "$1" ] || return

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
    [ -f "$1" ] || return

    io_linked_head "$1"
    io_list "$1"
}

io_output () {
    $@ > $io_temp
    io_status=$?
    stderr "  $io_status <- $@"

    echo
    if [ "$io_status" = 0 ]; then
        echo "Command \`$@\` produces:"
    else
        echo "Command \`$@\` exits with $io_status and produces:"
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
    for k in `io_real_files $@` output; do io_table_calc $k; done
    # Input
    for k in $@; do io_input $k; done
    # Output
    io_head output
    io_output $@
}

io_real_files () {
    for k in $@; do
        [ -f $k ] && echo $k
    done
}

io_body_file () {
    io_calc=$@

    # Table
    cat $io_glob_file | io_table
    # Input
    for k in $io_calc; do io_input $k; done
    # Output
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
        # exclude calc files
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

    if io_diff_cmd $io_output_orig $io_output_work > $io_temp; then
        io_diff_result OK
    else
        echo
        io_diff_result DIFF
        echo
        echo "Differences are found:"
        echo
        io_list $io_temp
        echo

        if [ $io_diff '<' 2 ]; then
            echo "To show all differences, please give more -d flags."
            echo "To examine this differences, type: cd `io_pwd`"
            echo
            io_status=2
            return
        fi
    fi
}

io_diff_cmd () {
    diff \
        --old-line-format=' - %.4dn | %L' \
        --new-line-format=' + %.4dn | %L' \
        --old-group-format='Deleted
%<
' \
        --new-group-format='Added
%>
' \
        --changed-group-format='Changed from
%<to
%>
' \
        --unchanged-group-format='' \
        "$1" "$2"
}

io_diff_result () {
    io_dir=`io_pwd`
    io_dir_md=`echo $io_dir | sed 's:/: / :g'`

    if [ $io_link_yn = y ]; then
        echo "* $1 – [$io_output_orig]($io_dir/$io_output_orig) in $io_dir_md"
    else
        echo "* $1 – $io_output_orig in $io_dir_md"
    fi
}

io_pwd () {
    if [ -d "$IO_TOP" ]; then
        io_top=`echo $IO_TOP | tr '[:punct:]' .`'/*'
        pwd | sed "s:^$io_top::"
    else
        pwd
    fi
}


# ============================================  Command line

io_cmd_line () {
    for arg in $*; do
        case $arg in
            -l)  ;;
            -d)  ;;
            -dd) ;;
            *)   echo $arg ;;
        esac
    done | xargs
}

io_cmd_check () {
    case "$io_output_work" in
        *.$io_glob_ext )
              stderr "  Output file is probably input file."
              stderr "  Please check: $io_output_work"
              exit 2 ;; 
    esac
}

io_cmd=`basename $0`
io_cmd_line=`io_cmd_line $io_cmd $*`
io_diff=0             # 0 | 1 | 2
io_error=output       # output | inhibit
io_glob_ext=k
io_glob_file=
io_glob_type=args     # args | glob | file
io_keep_temp_yn=n     # y | n
io_link_yn=n          # y | n
io_output_orig=
io_status=0

while getopts df:ghlo:rstx:V io_opt; do
    case $io_opt in
        d)  case $io_diff in
              2) io_diff=2 ;;
              1) io_diff=2 ;;  # show all differences
              *) io_diff=1 ;;  # show first differences shortly
            esac
            io_error=inhibit          ;;
        f)  io_glob_type=file
            io_glob_file=$OPTARG      ;;
        g)  io_glob_type=glob         ;;
        l)  io_link_yn=y              ;;
        o)  io_output_orig=$OPTARG    ;;
        r)  io_output_orig=README.md  ;;
        s)  io_output_orig=INOUT.md   ;;
        t)  io_keep_temp_yn=y         ;;
        x)  io_glob_ext=$OPTARG       ;;
        V)  io_version                ;;
        ?)  io_usage                  ;;
    esac
done

io_output_work=$io_output_orig
shift $(($OPTIND - 1))

[ $# = 0 ] && io_usage

stderr "$io_cmd_line"
io_cmd_check


# ============================================  Main

io_temp=`io_create_temporary`

if [ $io_diff = 0 ]; then
    io_doc $@
else
    io_diff $@
fi

# clean up

io_delete_temporary $io_temp

exit $io_status

