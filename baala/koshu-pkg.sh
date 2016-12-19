#!/bin/sh

pkg_help () {
    echo "DESCRIPTION"
    echo "  Operations for each packages"
    echo
    echo "USAGE"
    echo "  $pkg_prog COMMAND"
    echo
    echo "COMMAND for listing"
    echo "  baala-dir        Show baala directory"
    echo "  cabal            List cabal files"
    echo "  cabal-path       List paths of cabal files"
    echo "  copyright        List copyright in cabal files"
    echo "  dir              List package directory names"
    echo "  hoogle           List Hoogle files"
    echo "  hoogle P ...     Grep P ... for Hoogle files"
    echo "  import [DIR]     List import lines"
    echo "  import-outer [DIR] List imported modules except for Koshucode"
    echo "  installed        List installed packages in sandbox"
    echo "  installed-koshu  List installed packages named 'koshu'"
    echo "  synopsis         List synopses in cabal files"
    echo "  version          List version number in cabal files"
    echo
    echo "COMMAND for executing"
    echo "  alt X Y          Alter X to Y in cabal files"
    echo "  clean            Clean up build result"
    echo "  doc0 [DIR]       Generate Haddock documents less verbosely"
    echo "  doc [DIR]        Generate Haddock documents with default verbosity"
    echo "  doc2             Regenerate Haddock documents"
    echo "  exec C           Execute C in each package directories"
    echo "  exec-all C       Same as exec except for ignoring exit status"
    echo "  init             Initilize sandbox"
    echo "  unreg            Unregister koshucode libraries"
    echo
}

pkg_abort () {
    echo "ABORT / $*"
    exit 2
}

pkg_status () {
    echo "STATUS $pkg_status / $*"
}

pkg_section () {
    echo
    echo "***********  $*"
    echo
}

pkg_dirs () {
    echo overture subtext base syntax type data data-plus core writer \
        rop-flat rop-nested rop-cox cop calculator toolkit
}

pkg_dirs_rev () {
    echo toolkit calculator cop rop-cox rop-nested rop-flat \
        writer core data-plus data type syntax base subtext overture
}

pkg_dirs_or () {
    if [ -z "$1" ]; then
        pkg_dirs
    else
        echo "$1" | sed 's:/$::'
    fi
}

pkg_cabal () {
    for pkg in `pkg_dirs`; do
        echo "koshucode-baala-$pkg.cabal"
    done
}

pkg_cabal_path () {
    for pkg in `pkg_dirs`; do
        pkg_cabal="$pkg_dir/$pkg/koshucode-baala-$pkg.cabal"
        if [ -e "$pkg_cabal" ]; then
            echo "$pkg_cabal"
        fi
    done
}

pkg_cabal_section () {
    for pkg_cabal in `pkg_cabal_path`; do
        pkg_base=`basename "$pkg_cabal"`
        pkg_label=`printf "%-40s " "$pkg_base"`
        sed -n "s/^$1: */$pkg_label/p" "$pkg_cabal"
    done
}

pkg_cabal_alt () {
    if [ -z "$1" ] || [ -z "$2" ]; then
        echo "Skip alteration"
    else
        echo "Alter cabal files: $1 -> $2"
        for cab in `pkg_cabal_path`; do
            perl -i -pe "s/$1/$2/" $cab
        done
    fi
}

pkg_exec () {
    for pkg in `pkg_dirs`; do
        ( cd "$pkg"
          pkg_section "$* (in $pkg)"
          "$@"
        )
        pkg_status=$?
        if [ $pkg_status != 0 ]; then
            echo 
            abort "status $pkg_status"
        fi
    done
}

pkg_exec_all () {
    for pkg in `pkg_dirs`; do
        ( cd "$pkg"
          pkg_section "$* (in $pkg)"
          "$@"
        )
        pkg_status=$?
        if [ $pkg_status != 0 ]; then
            echo "ABORT / status $pkg_status"
        fi
    done
}

pkg_import () {
    for pkg in `pkg_dirs_or $1`; do
        ( cd "$pkg"
          pkg_section "$pkg"
          find Koshucode -name '*.hs' \
            -exec grep '^module \|^import' {} ';' \
            -exec echo ';'
        )
    done
}

pkg_import_outer () {
    for pkg in `pkg_dirs_or $1`; do
        ( cd "$pkg"
          pkg_section "$pkg"
          find Koshucode -name '*.hs' \
              -exec perl -lne 'print "$2" if /^import +(qualified *)?([^\s]+)/' {} ';' \
              | grep -v '^Koshucode' | sort | uniq | cat -n
        )
    done 
}

pkg_haddock () {
    for pkg in `pkg_dirs_or $1`; do
        ( cd "$pkg"
          pkg_section "haddock (in $pkg)"
          cabal haddock \
              --verbose=$pkg_doc_verbose \
              --html \
              --css=../haddock/ocean2.css \
              --hscolour-css=../haddock/hscolour2.css \
              --hoogle \
              --hyperlink-source \
              --html-location=$pkg_doc_hackage \
              --haddock-option=--no-warnings \
              --haddock-option=--pretty-html \
              `pkg_haddock_option $pkg`
          pkg_haddock_postproc "$pkg"
        )
    done
}

pkg_haddock_option () {
    for pkg in `pkg_dirs`; do
        if [ $pkg = $1 ]; then
            break
        fi
        pkg_name="koshucode-baala-$pkg"
        pkg_interface="../$pkg/dist/doc/html/$pkg_name/$pkg_name.haddock"
        if [ -e "$pkg_interface" ]; then
            echo "--haddock-option=--read-interface=$pkg_doc_koshu/$pkg_name,$pkg_interface"
        fi
    done
}

pkg_haddock_postproc () {(
    cd dist/doc/html

    # Source link
    for html in */*.html; do
        dir=`dirname $html`
        perl -i -p - <<EOF $html
s|^window.onload|function showSource (href) { window.parent.Haddock.showSource('$1', '$dir', href); }
window.onload|;
s/<a href="(src[^"]*)"/<a onclick="showSource('\1');" href="javascript:void(0);"/;
EOF
    done

    # Index
    for html in */doc-index*.html; do
        dir=`dirname $html`
        perl -i -p - <<EOF $html
s|Koshucode[.]Baala[.]|<strong>K</strong>.|g;
EOF
    done

    # Line number
    for html in */src/*.html; do
        perl -i -p - <<EOF $html
s|<a name="line-([^"]*)">|<a class="number" name="line-\1">\1|;
EOF
    done
)}

pkg_hoogle () {
    for pkg in `pkg_dirs`; do
        pkg_name="koshucode-baala-$pkg"
        pkg_hoogle=$pkg/dist/doc/html/$pkg_name/$pkg_name.txt
        if [ -e $pkg_hoogle ]; then
            echo $pkg_hoogle
        fi
    done
}

pkg_hoogle_grep () {
    for pkg in `pkg_hoogle`; do
        pkg_section "$pkg"
        grep -v "^--" $pkg | pkg_grep_$# "$@" | sort | cat -n
    done
}

g () { grep -i "$1" ; }

pkg_grep_1 () { g "$1" ; }
pkg_grep_2 () { g "$1" | g "$2" ; }
pkg_grep_3 () { g "$1" | g "$2" | g "$3" ; }
pkg_grep_4 () { g "$1" | g "$2" | g "$3" | g "$4" ; }
pkg_grep_5 () { g "$1" | g "$2" | g "$3" | g "$4" | g "$5" ; }

pkg_sandbox_init () {
    if [ -e cabal.sandbox.config ]; then
        echo "Sandbox is already initialized."
    else
        cabal sandbox init --sandbox ../cabal/sandbox
    fi
}

pkg_installed () {
    pkg_installed_list "$1" | pkg_installed_list_k
}

pkg_installed_list () {
    cabal exec ghc-pkg -- list "$1" --simple-output | xargs -n 1
}

# Convert version string to Koshucode.
pkg_installed_list_k () {
    perl -pe 's%(.*)-([\d.]+)$%|-- INSTALLED  /package "$1"  /version "$2"%'
}

pkg_unreg () {
    for pkg in `pkg_dirs_rev`; do
        cabal exec ghc-pkg unregister koshucode-baala-$pkg
        pkg_status=$?
        pkg_status "Unregister koshucode-baala-$pkg"
    done
}

# Execute command in the toolkit directory.
pkg_toolkit () {
    if [ -e "$pkg_dir/toolkit/cabal.sandbox.config" ]; then
        ( cd "$pkg_dir/toolkit" ;
          "$@"
        )
    fi
}

# --------------------------------------------  main

pkg_prog=koshu-pkg.sh
pkg_dir=`pwd | sed -n 's:\(.*/koshucode-master/baala\).*:\1:p'`

pkg_doc_koshu=http://seinokatsuhiro.github.io/koshucode/haddock/doc
pkg_doc_hackage='http://hackage.haskell.org/package/$pkg/docs'
pkg_doc_verbose=1

if [ ! -d "$pkg_dir" ]; then
    pkg_abort "No baala directory"
fi

case "$1" in

    # list
    baala-dir)
        echo "$pkg_dir" ;;
    cabal)
        pkg_cabal ;;
    cabal-path)
        pkg_cabal_path ;;
    copyright)
        pkg_cabal_section copyright ;;
    dir)
        pkg_dirs | xargs -n 1 ;;
    dir-rev)
        pkg_dirs_rev | xargs -n 1 ;;
    hoogle)
        shift
        if [ $# = 0 ]; then
            pkg_hoogle
        else
            pkg_hoogle_grep "$@"
        fi ;;
    import)
        pkg_import "$2" ;;
    import-outer)
        pkg_import_outer "$2" ;;
    installed)
        pkg_toolkit pkg_installed "*" ;;
    installed-koshu)
        pkg_toolkit pkg_installed "koshu*" ;;
    synopsis)
        pkg_cabal_section synopsis ;;
    version)
        pkg_cabal_section version ;;

    # execute
    alt)
        pkg_cabal_alt "$2" "$3" ;;
    clean)
        pkg_exec cabal clean ;;
    doc0)
        pkg_doc_verbose=0
        pkg_haddock "$2" 2> /dev/null ;;
    doc)
        pkg_haddock "$2" ;;
    doc2)
        pkg_exec cabal configure
        pkg_exec cabal build
        pkg_haddock ;;
    exec)
        shift
        pkg_exec "$@" ;;
    exec-all)
        shift
        pkg_exec_all "$@" ;;
    init)
        pkg_exec pkg_sandbox_init ;;
    unreg)
        pkg_toolkit pkg_unreg ;;

    # unknown
    *)
        pkg_help ;;
esac

