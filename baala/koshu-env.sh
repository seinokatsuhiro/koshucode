#!/bin/sh
#
#  EXAMPLES
#    $ koshu-env.sh ghci
#    $ koshu-env.sh runhaskell
#

for conf in $HOME/cabal-dev/koshucode/packages-*.conf; do
    GHC_PACKAGE_PATH=$conf:
done

export GHC_PACKAGE_PATH

"$@"

