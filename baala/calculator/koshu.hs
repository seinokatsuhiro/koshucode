#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Core                     as C
import qualified Koshucode.Baala.Toolkit.Library.Global   as Gl
import qualified Koshucode.Baala.Toolkit.Library.Version  as Ver
import qualified Koshucode.Baala.Toolkit.Main.KoshuMain   as Main

main :: IO ()
main = B.exitWith =<< Main.koshuMain koshuGlobal

koshuGlobal :: C.GlobalC
koshuGlobal = Gl.baalaGlobal
              { C.globalSynopsis  = "The Koshucode Baala Implementation"
              , C.globalVersion   = Ver.version }

