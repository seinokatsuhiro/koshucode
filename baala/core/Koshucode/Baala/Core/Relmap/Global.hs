{-# OPTIONS_GHC -Wall #-}

-- | Global parameters.

module Koshucode.Baala.Core.Relmap.Global
( Global' (..),
  globalCommandLine,
  globalFill,
  globalRops,
  globalCops,
  globalCopset,
  globalInfix,
  global,

  OpSet' (..),
  opset,
  opsetFill,
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

globalRops   :: Global' rop c -> [rop c]
globalRops   = opsetRopList . globalOpset

globalCops   :: Global' rop c -> [C.Cop c]
globalCops   = C.copsetCopList . opsetCop . globalOpset

globalInfix  :: Global' rop c -> [B.Named B.InfixHeight]
globalInfix  = C.copsetInfixList . opsetCop . globalOpset

globalCopset :: Global' rop c -> C.CopSet c
globalCopset = opsetCop . globalOpset

global :: Global' rop c
global = Global
         { globalVersion  =  D.Version [] []
         , globalOpset    =  opset
         , globalProgram  =  ""
         , globalArgs     =  []
         , globalJudges   =  []
         , globalSelect   =  \_ _ -> B.reldee }


-- ----------------------  OpSet

data OpSet' rop c = OpSet
    { opsetRopList  :: [rop c]
    , opsetCop      :: C.CopSet c
    }

opset :: OpSet' rop c
opset = OpSet [] C.copset

opsetFill :: B.Map (OpSet' rop c)
opsetFill ops = ops2 where
    ops2  = ops { opsetCop = copset2 }
    copset2 = C.copsetFill $ opsetCop ops

