{-# OPTIONS_GHC -Wall #-}

-- | Global parameters.

module Koshucode.Baala.Core.Relmap.Global
  ( -- * Global
    Global' (..),
    globalCommandLine,
    globalFill,
    globalRops,
    globalCops, globalCopset,
    globalInfix,
    global',

    GetGlobal (..),
    ropGlobal,
    ropCopset,
  
    -- * Operator set
    OpSet' (..),
    opset, opsetFill,
  ) where

import qualified Data.Version                        as D
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Core.Content        as C
import qualified Koshucode.Baala.Core.Lexmap         as C
import qualified Koshucode.Baala.Core.Relmap.Rop     as C


-- ----------------------  GetGlobal

class GetGlobal h where
    getGlobal :: h c -> Global' h c

ropGlobal :: (GetGlobal h) => C.RopUse' h c -> Global' h c
ropGlobal = getGlobal . C.ropHook

-- | Get operator set from 'RopUse'.
ropCopset :: (GetGlobal h) => C.RopUse' h c -> C.CopSet c
ropCopset = globalCopset . ropGlobal


-- ----------------------  Global

-- | Global parameters
data Global' h c = Global
      { globalVersion      :: D.Version
      , globalOpset        :: OpSet' (C.Rop' h) c
      , globalProgram      :: String
      , globalArgs         :: [String]
      , globalProxy        :: [B.HttpProxy]
      , globalTime         :: B.Time
      , globalSourceCount  :: Int
      , globalSources      :: [B.Source]
      , globalJudges       :: [B.Judge c]
      , globalHook         :: h c
      }

instance Show (Global' h c) where
    show Global { globalVersion = ver }
        = "Global (" ++ show ver ++ ")"

globalCommandLine :: Global' h c -> [String]
globalCommandLine Global { globalProgram = prog, globalArgs = args }
    = prog : args

globalFill :: (C.CContent c) => B.Map (Global' h c)
globalFill g = g

globalRops   :: Global' h c -> [C.Rop' h c]
globalRops    = opsetRopList . globalOpset

globalCops   :: Global' h c -> [C.Cop c]
globalCops    = C.copsetCopList . opsetCop . globalOpset

globalInfix  :: Global' h c -> [B.Named B.InfixHeight]
globalInfix   = C.copsetInfixList . opsetCop . globalOpset

globalCopset :: Global' h c -> C.CopSet c
globalCopset  = opsetCop . globalOpset

-- | Empty global parameters.
global' :: h c -> Global' h c
global' h = Global
    { globalVersion      = D.Version [] []
    , globalOpset        = opset
    , globalProgram      = ""
    , globalArgs         = []
    , globalProxy        = []
    , globalTime         = B.timeFromMjd 0
    , globalSourceCount  = 0
    , globalSources      = []
    , globalJudges       = []
    , globalHook         = h }


-- ----------------------  Operator set

-- | Set of relmap and content operators.
data OpSet' rop c = OpSet
    { opsetRopList     :: [rop c]
    , opsetFindRop     :: C.RopName -> Maybe (rop c)
    , opsetCop         :: C.CopSet c
    }

-- | Empty operator set.
opset :: OpSet' rop c
opset = OpSet [] find C.copset where
    find _ = Nothing

opsetFill :: (B.Name (rop c)) => B.Map (OpSet' rop c)
opsetFill ops = ops2 where
    ops2      = ops { opsetFindRop = findRop
                    , opsetCop     = copset2 }
    copset2   = C.copsetFill $ opsetCop ops
    findRop   = B.assocFinder rops
    rops      = map name $ opsetRopList ops
    name rop  = (B.name rop, rop)

