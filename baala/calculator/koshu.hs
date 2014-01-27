#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop
import qualified Koshucode.Baala.Minimal as Rop
import qualified Koshucode.Baala.Vanilla as Rop
import qualified Koshucode.Baala.Toolkit.Main.KoshuMain   as Main
import qualified Koshucode.Baala.Toolkit.Library.Exit     as Main
import qualified Koshucode.Baala.Toolkit.Library.Version  as Main

main :: IO ()
main = Main.exit =<< Main.koshuMain g where
    g = C.global { C.globalVersion = Main.version
                 , C.globalCops    = Rop.vanillaCops
                 , C.globalRops    = Rop.builtinRops ++
                                     Rop.minimalRops ++
                                     Rop.vanillaRops }

