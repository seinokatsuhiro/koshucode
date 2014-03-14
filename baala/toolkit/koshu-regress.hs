#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Toolkit.Main.KoshuRegress as Main
import qualified Koshucode.Baala.Toolkit.Library.Exit      as Main
import qualified Koshucode.Baala.Core  as C
import qualified Koshucode.Baala.Op    as Op

main :: IO ()
main =
    do status <- Main.koshuRegressMain rops
       Main.exit status
    where
      rops :: [C.Rop Op.VContent]
      rops = concat [ Op.builtinRops
                    , Op.minimalRops
                    , Op.vanillaRops ]
                                
