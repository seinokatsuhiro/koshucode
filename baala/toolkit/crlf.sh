#!/bin/sh

crlf_help () {
    echo "DESCRIPTION"
    echo "  Convert newlines from LF to CRLF"
    echo
    echo "USAGE"
    echo "  $crlf_prog [OPTIONS] FILE ..."
    echo
    echo "OPTIONS"
    echo "  -l   List files with symbols LF or CRLF"
    echo "  -n   List files only whose newline is LF (\\\n)"
    echo "  -r   List files only whose newline is CRLF (\\\r\\\n)"
    echo "  -c   Change file by converting LF to CRLF"
    echo "  -b   Change file backward, that is CRLF to LF"
    echo "  -h   Print this message"
    echo
    echo "EXAMPLES"
    echo "  $crlf_prog *.txt               # list newline is LF or CRLF"
    echo "  $crlf_prog -c *.txt            # convert LF to CRLF"
    echo "  find . -name '*.txt' \\"
    echo "    | xargs -n 10 $crlf_prog     # list all files named '*.txt'"
    echo
}

crlf_abort () {
    echo
    echo "ABORTED"
    exit 2
}

# --------------------------------------------  List

crlf_contain () {
    perl -ne 'exit 1 if /\r\n/' "$1"

    case $? in
        0) return 1   ;;
        1) return 0   ;;
        *) crlf_abort ;;
    esac
}

crlf_list_all () {
    if crlf_contain "$1"; then
        # newline is CRLF
        echo "CRLF  $1"
    else
        # newline is LF, or file has no newlines
        echo "LF    $1"
    fi
}

crlf_list_crlf () {
    if crlf_contain "$1"; then
        echo "$1"
    fi
}

crlf_list_lf () {
    if ! crlf_contain "$1"; then
        echo "$1"
    fi
}

# --------------------------------------------  Change

crlf_add    () { perl -i -pe 's/\n/\r\n/' "$1" ; }
crlf_delete () { perl -i -pe 's/\r\n/\n/' "$1" ; }

crlf_add_necessary () {
    if ! crlf_contain "$1"; then
        crlf_add "$1"
        if [ $? = 0 ]; then
            echo "Convert LF to CRLF : $1"
        else
            crlf_abort
        fi
    fi
}

crlf_delete_necessary () {
    if crlf_contain "$1"; then
        crlf_delete "$1"
        if [ $? = 0 ]; then
            echo "Convert CRLF to LF : $1"
        else
            crlf_abort
        fi
    fi
}

# --------------------------------------------  Main

crlf_prog=`basename $0`
crlf_mode=list

while getopts bchlnr crlf_opt; do
    case $crlf_opt in
        b) crlf_mode=delete  ;;
        c) crlf_mode=add     ;;
        l) crlf_mode=list    ;;
        n) crlf_mode=lf      ;;
        r) crlf_mode=crlf    ;;
        *) crlf_help         ;;
    esac
done

shift $(($OPTIND - 1))

for f in "$@"; do
    case $crlf_mode in
        list)     crlf_list_all         "$f" ;;
        lf)       crlf_list_lf          "$f" ;;
        crlf)     crlf_list_crlf        "$f" ;;
        add)      crlf_add_necessary    "$f" ;;
        delete)   crlf_delete_necessary "$f" ;;
        *)        crlf_abort                 ;;
    esac
done
