#!/bin/sh
#
#  Copy Haddock documents from master directory.
#

rm -rf koshucode-*

for dir in ../../../koshucode-master/baala/*/dist/doc/html/*; do
    echo "Copy $dir"
    cp -r $dir .
    if [ $? != 0 ]; then
        echo "ERROR"
        exit 2
    fi
done

for dir in koshucode-*; do
    cp image/*.gif $dir
    cp image/*.png $dir
done
