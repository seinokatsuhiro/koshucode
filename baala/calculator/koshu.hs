#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified System.Environment                       as Env
import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Core                     as C
import qualified Koshucode.Baala.Toolkit.Library.Global   as Gl
import qualified Koshucode.Baala.Toolkit.Library.Version  as Ver
import qualified Koshucode.Baala.Toolkit.Main.KoshuMain   as Main

main :: IO ()
main = B.exitWith =<< Main.koshuMain koshuGlobal

-- The main function with command-line argument.
-- To debug main function using GHCi:
--
-- > :l "koshu.hs"
-- > :trace mainWith ["INPUT.k", ...]
--
mainWith :: [String] -> IO ()
mainWith args = Env.withArgs args main

koshuGlobal :: C.GlobalC
koshuGlobal = Gl.baalaGlobal
              { C.globalSynopsis  = "The Koshucode Baala Implementation"
              , C.globalVersion   = Ver.version }

