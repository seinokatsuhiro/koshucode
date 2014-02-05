#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Vanilla as Rop
import qualified Koshucode.Baala.Toolkit.Main.KoshuMain   as Main
import qualified Koshucode.Baala.Toolkit.Library.Exit     as Main
import qualified Koshucode.Baala.Toolkit.Library.Version  as Main

main :: IO ()
main = Main.exit =<< Main.koshuMain koshuGlobal

koshuGlobal :: C.Global Rop.VContent
koshuGlobal = Rop.vanillaGlobal { C.globalVersion = Main.version }

