#!/bin/sh
#
#  DESCRIPTION
#    Simple regression test
#

regress_usage () {
    echo "DESCRIPTION"
    echo "  Utility for simple regression test"
    echo
    echo "USAGE"
    echo "  regress.sh EXECUTABLE ... -l"
    echo "      Execute regression test"
    echo "      Output to REGRESS/last/*.log"
    echo
    echo "  regress.sh -s"
    echo "      Save the last result"
    echo "      Output to REGRESS/save/*.log"
    echo
    echo "  regress.sh -c"
    echo "      Clear REGRESS directory"
    echo
    exit
}



# =================================  Utility

regress_abort () {
    echo
    echo "**"
    echo "**  ABORT - $*"
    echo "**"
    exit 1
}

regress_section () {
    echo "-- ---------------------------------  $*"
    echo
}

regress_arg () {
    which="$1"
    shift
    for x in "$@"; do
        case "$which:$x" in
            opt:-*  ) echo "$x" ;;
            file:-* ) ;;
            file:*  ) echo "$x" ;;
        esac
    done
}



# =================================  Do test

regress_last_all () {
    sum_path=REGRESS/SUMMARY.k
    /bin/mkdir -p REGRESS/last
    regress_result_head > "$sum_path"

    for x in "$@" ; do
        if [ -e ./"$1" ]; then
            regress_last "$x"
        else
            regress_abort "File not exists: $x"
        fi
    done

    regress_section "$sum_path"
    /bin/cat "$sum_path"
}

regress_last () {
    last_path=REGRESS/last/"$1".log
    save_path=REGRESS/save/"$1".log

    regress_section "$1"

    ./"$1" > "$last_path"
    if [ $? != 0 ]; then
        regress_abort "Execution failed: $1"
    fi

    /bin/cat "$last_path"

    if [ -e "$save_path" -a -e "$last_path" ]; then
        /usr/bin/diff -c "$save_path" "$last_path"
        if [ $? = 0 ]; then
            regress_result "good" "$1"
        else
            regress_result "bad " "$1"
            regress_abort "Unmatches are found"
        fi
    fi
}

regress_result_head () {
    echo "** -*- koshu -*-"
    echo "**"
    echo "**  DESCRIPTION"
    echo "**    Report for a regression test."
    echo "**"
    echo

    echo "|-- REGRESS-DATETIME  /date '$reg_date  /time '$reg_time"
}

regress_result () {
    echo "|-- REGRESS-RESULT  /result '$1  /input '$2" >> "$sum_path"
}



# =================================  Save

regress_save () {
    if [ -d REGRESS/last ]; then
        /bin/rm -Rf REGRESS/save
        /bin/cp -R  REGRESS/last REGRESS/save
    else
        regress_abort "Directory not exists: REGRESS/last"
    fi
}



# =================================  Option

regress_opt () {
    while getopts cdhls OPT; do 
        case $OPT in
            c) reg_clean=yes ;;
            d) reg_desc=yes  ;;
            l) reg_last=yes  ;;
            s) reg_save=yes  ;;
            *) regress_usage ;;
        esac
    done
}

regress_desc () {
    echo "OPTIONS"
    echo "  $reg_opt"
    echo
    echo "FILES"
    echo "  $reg_nonopt"
    echo
    echo "SETTING"
    echo "  reg_desc  $reg_desc"
    echo "  reg_clean $reg_clean"
    echo "  reg_last  $reg_last"
    echo "  reg_save  $reg_save"
    echo "  reg_date  $reg_date"
    echo "  reg_time  $reg_time"
    echo
    exit
}



# =================================  Main

reg_clean=no
reg_last=no
reg_save=no
reg_desc=no
reg_date=`/bin/date +%Y.%d.%m`
reg_time=`/bin/date +%H:%M:%S`

reg_opt=`regress_arg opt $*`
reg_nonopt=`regress_arg file $*`
regress_opt $reg_opt

[ $reg_desc  = yes ] && regress_desc
[ $reg_clean = yes ] && /bin/rm -R REGRESS
[ $reg_last  = yes ] && regress_last_all $reg_nonopt
[ $reg_save  = yes ] && regress_save

exit 0

