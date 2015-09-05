#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Toolkit.Main.KoshuChange as K

main :: IO ()
main = do status <- K.koshuChangeMain
          B.exitWith status

