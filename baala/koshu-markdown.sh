#!/bin/sh

md_version () {
    echo "koshu-markdown-0.50"
    exit
}

md_usage () {
    echo "DESCRIPTION"
    echo "  Generate markdown document for koshu scripts."
    echo
    echo "USAGE"
    echo "  $md_cmd [OPTION ...] SCRIPT.k ..."
    echo
    echo "OPTION"
    echo "  -d          show differences from last document"
    echo "  -f FILE     take input files from FILE"
    echo "  -g          glob input files by *.$md_glob_ext"
    echo "  -h          print help message"
    echo "  -o FILE.md  save document to FILE.md"
    echo "  -p PROG     use PROGram instead of $md_program"
    echo "  -r          save document to README.md"
    echo "  -x EXT      use EXTension instead of *.$md_glob_ext"
    echo
    echo "EXAMPLE"
    echo "  $md_cmd -r -g CALC.k"
    echo "  $md_cmd -o CALC.md -f FILE CALC.k"
    echo "  $md_cmd CALC.k DATA.k"
    echo
    exit
}

error () {
    if [ $md_error = output ]; then
        echo "$*" 1>&2
    fi
}


# ============================================  Document

md_doc () {
    if [ -z "$md_output" ]; then
        md_doc_body $@
    else
        md_doc_body $@ > $md_output
        error "  Output to $md_output"
    fi
}

md_doc_body () {
    md_title
    md_body $@
    md_trailer
}

check_output () {
    case "$md_output" in
        *.$md_glob_ext )
              error "  Output file is probably input file."
              error "  Please check: $md_output"
              exit 2 ;; 
    esac
}

# ============================================  Table of contents

md_link       () { echo "(#$@)" | tr -d . | tr ' ' '-' \
                                | tr '[:upper:]' '[:lower:]'; }
md_table_calc () { echo "- [$@]`md_link $@`" ; }
md_table_data () { echo "- $md_program $md_calc [$@]`md_link "$@"`" ; }
md_table_from () { echo "$@" | xargs -n 1 | md_table ; }

md_table () {
    for k in $md_calc ; do md_table_calc $k ; done
    while read k      ; do md_table_data $k ; done
}


# ============================================  Heading

md_title () {
    echo "# I/O list of $md_program"
    echo
}

md_head () {
    echo
    echo
    echo
    echo "## $*"
    echo
}

md_linked_head () {
    md_head "[$1]($1)"
}

md_trailer () {
    md_head "command"

    echo "This document is produced by the command:"
    echo
    echo '```'
    echo "$md_cmdline"
    echo '```'
}


# ============================================  Input / Output

md_list () {
    echo '```'
    tr -d '\r' < "$1"
    echo '```'
}

md_list_all () {
    for k in $@; do
        md_list $k
    done
}

md_input () {
    if [ -f "$1" ]; then
        md_linked_head "$1"
        md_list "$1"
    fi
}

md_output () {
    $md_program "$@" > $md_temp
    md_status=$?
    error "  $md_status <- $md_program $@"

    echo
    if [ "$md_status" = 0 ]; then
        echo "Command \`$md_program $@\` produces:"
    else
        echo "Command \`$md_program $@\` exits with $md_status and produces:"
    fi

    echo
    md_list $md_temp
}


# ============================================  Contents

md_body () {
    case $md_glob_type in
        args) md_body_args $@ ;;
        file) md_body_file $@ ;;
        glob) md_body_glob $@ ;;
    esac
}

md_body_args () {
    # Table
    for k in $@ output; do md_table_calc $k; done
    # Input
    for k in $@; do md_input $k; done
    # Output
    md_head output
    md_output $@
}

md_body_file () {
    md_calc=$@

    # Table
    cat $md_glob_file | md_table
    # Calc
    for k in $md_calc; do md_input $k; done
    # Data
    cat $md_glob_file | while read md_data; do
        md_head $md_data
        md_list_all $md_data
        md_output $md_calc $md_data
    done
}

md_body_glob () {
    md_calc=$@
    md_data=`md_glob_data`

    # Table
    md_table_from $md_data
    # Calc
    for k in $md_calc; do md_input $k; done
    # Data
    for k in $md_data; do
        md_input $k
        md_output $md_calc $k
    done
}

md_glob_data () {(
    for k in *.$md_glob_ext; do
        if ! md_exist $k $md_calc; then
            echo $k
        fi
    done
)}

md_exist () {(
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

md_diff () {
    if [ -f "$md_output" ]; then
        md_diff_body $@
        [ -f $md_output ] && rm $md_output
    else
        echo "file not found: $md_output"
        exit 2
    fi
}

md_diff_body () {
    md_output_original=$md_output
    md_output=`mktemp TEMP-KOSHU-DIFF-XXXXX`

    md_doc $@

    if diff -u $md_output_original $md_output > $md_temp; then
        md_diff_result OK
    else
        echo
        md_diff_result DIFF
        echo
        echo "**  Differences are found"
        echo "**"
        sed 's/^/**  /' < $md_temp
        echo

        if [ $md_diff = 1 ]; then
            echo "**  To show all differences, please give more -d flag."
            echo "**  To examine this differences, cd `md_pwd`"
            md_status=2
            return
        fi
    fi
}

md_diff_result () {
    echo "|-- $1  /dir \"`md_pwd`\"  /cmd \"$md_cmdline\""
}

md_pwd () {
    if [ -d "$MD_TOP" ]; then
        md_top=`echo $MD_TOP | tr '[:punct:]' .`'/*'
        pwd | sed "s:^$md_top::"
    else
        pwd
    fi
}

md_diff_del () {
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

md_cmd=`basename $0`
md_cmdline=`md_diff_del $md_cmd $*`
md_diff=0
md_error=output
md_glob_ext=k
md_glob_file=
md_glob_type=args
md_output=
md_program=koshu
md_status=0

# option

while getopts df:gho:p:rx:V opt; do
    case $opt in
        d)  case $md_diff in
              1) md_diff=2 ;;
              *) md_diff=1 ;;
            esac
            md_error=inhibit     ;;
        f)  md_glob_type=file
            md_glob_file=$OPTARG ;;
        g)  md_glob_type=glob    ;;
        o)  md_output=$OPTARG    ;;
        p)  md_program=$OPTARG   ;;
        r)  md_output=README.md  ;;
        x)  md_glob_ext=$OPTARG  ;;
        V)  md_version           ;;
        ?)  md_usage             ;;
    esac
done

shift $(($OPTIND - 1))

# document

error "$md_cmdline"
check_output
md_temp=`mktemp TEMP-KOSHU-XXXXX`

if [ $md_diff = 0 ]; then
    md_doc $@
else
    md_diff $@
fi

# clean up

[ -f $md_temp ] && rm $md_temp

exit $md_status
