#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop
import qualified Koshucode.Baala.Minimal as Rop
import qualified Koshucode.Baala.Vanilla as Rop
import qualified Koshucode.Baala.Toolkit.Main.KoshuMain as Main
import qualified Koshucode.Baala.Toolkit.Library.Exit   as Main

main :: IO ()
main =
    do status <- Main.koshuMain g
       Main.exit status
    where
      g = C.global { C.globalRops = rops
                   , C.globalCops = Rop.vanillaCops }
      rops = concat [ Rop.builtinRops
                    , Rop.minimalRops
                    , Rop.vanillaRops ]
                                
