{-# OPTIONS_GHC -Wall #-}

-- | Global parameters.

module Koshucode.Baala.Core.Relmap.Global
( GlobalWith (..),
  globalCommandLine,
  globalFill,
  global,
  globalSyntax,
  globalFunction,
) where

import qualified Data.Version                           as D
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content           as C
import qualified Koshucode.Baala.Core.Relmap.Relkit     as C


data GlobalWith rop c = Global
      { globalVersion :: D.Version
      , globalRops    :: [rop c]
      , globalCops    :: ([C.Cop c], [B.Named B.InfixHeight])
      , globalProgram :: String
      , globalArgs    :: [String]
      , globalJudges  :: [B.Judge c]
      , globalSelect  :: C.RelSelect c
      }

instance Show (GlobalWith rop c) where
    show Global { globalRops = rops, globalCops = (cops, _) }
        = let nr = length rops
              nc = length cops
          in "Global (" ++ show nr ++ " rops, " ++ show nc ++ " cops)"

globalCommandLine :: GlobalWith rop c -> [String]
globalCommandLine Global { globalProgram = prog, globalArgs = args }
    = prog : args

globalFill :: (C.CContent c) => B.Map (GlobalWith rop c)
globalFill g = g

global :: GlobalWith rop c
global = Global
         { globalVersion  =  D.Version [] []
         , globalRops     =  []
         , globalCops     =  ([], [])
         , globalProgram  =  ""
         , globalArgs     =  []
         , globalJudges   =  []
         , globalSelect   =  \_ _ -> B.reldee }

globalSyntax :: GlobalWith rop c -> C.CoxSyntax c
globalSyntax g = (syn, htab) where
    syn  = filter C.isCopSyntax $ fst cops
    htab = snd cops
    cops = globalCops g

globalFunction :: GlobalWith rop c -> [C.Cop c]
globalFunction = filter C.isCopFunction . fst . globalCops

