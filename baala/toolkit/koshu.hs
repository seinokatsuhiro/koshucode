#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Toolkit.Main.KoshuMain
import qualified Koshucode.Baala.Vanilla as V

main :: IO ()
main = koshuMain V.vanillaRelmaps

