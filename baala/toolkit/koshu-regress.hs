#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Toolkit.Main.KoshuRegress as Main
import qualified Koshucode.Baala.Toolkit.Library.Exit      as Main
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Minimal as Op
import qualified Koshucode.Baala.Op.Vanilla as Op

main :: IO ()
main =
    do status <- Main.koshuRegressMain rops
       Main.exit status
    where
      rops = concat [ Op.builtinRops
                    , Op.minimalRops
                    , Op.vanillaRops ]
                                
