#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Toolkit.Main.KoshuChange as K
import qualified Koshucode.Baala.Toolkit.Library.Exit     as K

main :: IO ()
main = do status <- K.koshuChangeMain
          K.exit status

