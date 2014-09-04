{-# OPTIONS_GHC -Wall #-}

-- | Global parameters.

module Koshucode.Baala.Core.Relmap.Global
( Global' (..),
  globalCommandLine,
  globalFill,
  globalRops,
  globalCops,
  globalInfix,
  globalFunction,
  globalSyntax,
  global,

  OpSet' (..),
) where

import qualified Data.Version                           as D
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content           as C
import qualified Koshucode.Baala.Core.Relmap.Relkit     as C


-- ----------------------  Global

data Global' rop c = Global
      { globalVersion :: D.Version
      , globalOpset   :: OpSet' rop c
      , globalProgram :: String
      , globalArgs    :: [String]
      , globalJudges  :: [B.Judge c]
      , globalSelect  :: C.RelSelect c
      }

instance Show (Global' opset c) where
    show Global { globalVersion = ver }
        = "Global (" ++ show ver ++ ")"

globalCommandLine :: Global' rop c -> [String]
globalCommandLine Global { globalProgram = prog, globalArgs = args }
    = prog : args

globalFill :: (C.CContent c) => B.Map (Global' rop c)
globalFill g = g

globalRops  :: Global' rop c -> [rop c]
globalRops  = opsetRops . globalOpset

globalCops  :: Global' rop c -> [C.Cop c]
globalCops  = opsetCops . globalOpset

globalInfix :: Global' rop c -> [B.Named B.InfixHeight]
globalInfix = opsetInfix . globalOpset

globalFunction :: Global' rop c -> [C.Cop c]
globalFunction = filter C.isCopFunction . opsetCops . globalOpset

globalSyntax :: Global' rop c -> C.CoxSyntax c
globalSyntax g = (syn, htab) where
    syn    =  filter C.isCopSyntax cops
    htab   =  opsetInfix opset
    cops   =  opsetCops  opset
    opset  =  globalOpset g

global :: Global' rop c
global = Global
         { globalVersion  =  D.Version [] []
         , globalOpset    =  B.mempty
         , globalProgram  =  ""
         , globalArgs     =  []
         , globalJudges   =  []
         , globalSelect   =  \_ _ -> B.reldee }


-- ----------------------  OpSet

data OpSet' rop c = OpSet
    { opsetRops  :: [rop c]
    , opsetCops  :: [C.Cop c]
    , opsetInfix :: [B.Named B.InfixHeight]
    }

instance B.Monoid (OpSet' rop c) where
    mempty  = OpSet [] [] []
    mappend = opsetAppend

opsetAppend :: OpSet' rop c -> OpSet' rop c -> OpSet' rop c
opsetAppend (OpSet rops1 cops1 infix1) (OpSet rops2 cops2 infix2)
    = OpSet rops3 cops3 infix3
      where rops3   =  rops1  ++ rops2
            cops3   =  cops1  ++ cops2
            infix3  =  infix1 ++ infix2

