This is a directory for sandbox hackage database.
All packages of koshucode share this sandbox.
To build the program `koshu`, please type the following commands.


For Unix-like system
------------------------------------------------------------------

Move to the working directory.

    cd koshucode/baala

Make a build script executable.

    chmod 755 cabal-koshu.sh

Setup a shared sandbox.

    ./cabal-koshu.sh sandbox-init
    ./cabal-koshu.sh sandbox-deps

Build the koshu command.

    ./cabal-koshu.sh link

    cabal/install.link koshu

    ls cabal/sanbox/bin



For Microsoft Windows
------------------------------------------------------------------


