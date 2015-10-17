#!/bin/sh

grep_abort () {
    grep 'Left.*Abort'
}

for hs in `find . -name '*.hs'`; do
    if cat "$hs" | grep_abort > /dev/null; then
        echo "$hs" | sed 's|^./||'
        cat -n "$hs" | grep_abort | sed 's/  */ /g'
    fi
done

