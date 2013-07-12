#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Toolkit.Main.KoshuRegress
import Koshucode.Baala.Vanilla as V

main :: IO ()
main = koshuRegressMain V.vanillaOperators

