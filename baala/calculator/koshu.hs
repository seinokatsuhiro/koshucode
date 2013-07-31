#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Toolkit.Main.KoshuMain as Main
import qualified Koshucode.Baala.Builtin as Rop
import qualified Koshucode.Baala.Minimal as Rop
import qualified Koshucode.Baala.Vanilla as Rop

main :: IO ()
main = Main.koshuMain $
       Rop.builtinOperators
       ++ Rop.minimalOperators
       ++ Rop.vanillaOperators

