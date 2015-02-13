#!/bin/sh

crlf_help () {
    echo "DESCRIPTION"
    echo "  Convert newlines from LF to CRLF"
    echo
    echo "USAGE"
    echo "  $crlf_prog [-hlnru] FILE ..."
    echo
    echo "OPTION"
    echo "  -l   List files with symbols LF or CRLF"
    echo "  -n   List files only whose newline is LF (\\\n)"
    echo "  -r   List files only whose newline is CRLF (\\\r\\\n)"
    echo "  -u   Update file by converting LF to CRLF"
    echo "  -h   Print this message"
    echo
}

crlf_abort () {
    echo
    echo "ABORTED"
    return 2
}

crlf_not_contain () {
    perl -ne 'exit 1 if /\r\n/' "$1"
    status=$?

    case $status in
        0) return 0   ;;
        1) return 1   ;;
        *) crlf_abort ;;
    esac
}

crlf_add () {
    perl -i -pe 's/\n/\r\n/' "$1"
}

crlf_add_necessary () {
    if crlf_not_contain "$1"; then
        crlf_add "$1"
        if [ $? = 0 ]; then
            echo "Convert LF to CRLF : $1"
        else
            crlf_abort
        fi
    fi
}

crlf_list_all () {
    if crlf_not_contain "$1"; then
        # newlne is LF
        # file that has no newline, especially empty file
        echo "LF    $1"
    else
        # newlne is CRLF
        echo "CRLF  $1"
    fi
}

crlf_list_lf () {
    if crlf_not_contain "$1"; then
        echo "$1"
    fi
}

crlf_list_crlf () {
    if crlf_not_contain "$1"; then
        :
    else
        echo "$1"
    fi
}

crlf_prog=`basename $0`
crlf_mode=list

while getopts hnru crlf_opt; do
    case $crlf_opt in
        n) crlf_mode=lf     ;;
        r) crlf_mode=crlf   ;;
        u) crlf_mode=update ;;
        *) crlf_help        ;;
    esac
done

shift $(($OPTIND - 1))

for f in "$@"; do
    case $crlf_mode in
        list)    crlf_list_all      "$f" ;;
        lf)      crlf_list_lf       "$f" ;;
        crlf)    crlf_list_crlf     "$f" ;;
        update)  crlf_add_necessary "$f" ;;
        *)       crlf_abort ;;
    esac
done
