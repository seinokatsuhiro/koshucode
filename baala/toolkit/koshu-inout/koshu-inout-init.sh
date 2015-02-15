#!/bin/sh

io_version () {
    echo "koshu-inout-init-0.68"
    exit
}

io_help () {
    echo "DESCRIPTION"
    echo "  Generate script for I/O list"
    echo
    echo "USAGE"
    echo "  $io_cmd [OPTION ...] SUBOPTION ..."
    echo
    echo "OPTION"
    echo "  -f FILE     generate FILE instead of $io_file"
    echo "  -h          print help message"
    echo "  -o          overwrite existing $io_file"
    echo "  -r          set report file be README.md"
    echo "  -V          print version"
    echo
    echo "SUBOPTION"
    echo "  Options for koshu-inout.sh"
    echo "  If omitted, default suboption is used: -- -s -g koshu"
    echo
    exit
}


# ============================================  Generate INOUT.sh

io_init () {
    if [ -f $io_file ]; then
        if [ $io_overwrite = y ]; then
            /bin/rm $io_file
            [ $? = 0 ] || exit 2
        else
            echo "$io_file already exists, please use -o option to overwrite."
            io_show_file
            exit 2
        fi
    fi

    io_content > $io_file

    if [ $? = 0 ]; then
        echo "$io_file is generated."
        io_show_file
        /bin/chmod 755 $io_file
    else
        echo "$io_file is not generated."
        exit 2
    fi
}

io_content () {
    echo "#!/bin/sh"
    echo "koshu-inout.sh \$* $io_arg"
}

io_show_file () {
    [ -f $io_file ] || return
    echo
    echo "----------  $io_file  ----------"
    /bin/cat $io_file
    echo
}


# ============================================  Command line

io_cmd=`basename $0`
io_file=INOUT.sh
io_overwrite=n
io_arg=
io_arg_output=-s  # INOUT.md

while getopts f:horV io_opt; do
    case $io_opt in
        f)  io_file=$OPTARG     ;;
        o)  io_overwrite=y      ;;
        r)  io_arg_output=-r    ;;  # README.md
        V)  io_version          ;;
        *)  io_help             ;;
    esac
done

shift $(($OPTIND - 1))

# decide $io_arg
if [ $# = 0 ]; then
    io_arg="$io_arg_output -g koshu"
elif [ "$1" = "--" ]; then
    shift
    io_arg="$*"
else
    io_arg="$*"
fi

# ============================================  Main

io_init

