#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Core                     as C
import qualified Koshucode.Baala.Type.Vanilla             as Type
import qualified Koshucode.Baala.Op.Global                as Op
import qualified Koshucode.Baala.Toolkit.Main.KoshuMain   as Main
import qualified Koshucode.Baala.Toolkit.Library.Exit     as Main
import qualified Koshucode.Baala.Toolkit.Library.Version  as Main

main :: IO ()
main = Main.exit =<< Main.koshuMain koshuGlobal

koshuGlobal :: C.Global Type.VContent
koshuGlobal = Op.vanillaGlobal
              { C.globalSynopsis  = "The Koshucode Baala Implementation"
              , C.globalVersion   = Main.version }

