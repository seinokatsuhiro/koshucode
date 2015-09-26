{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Global parameters.

module Koshucode.Baala.Core.Relmap.Global
  ( -- * Global parameter
    Global' (..),
    globalVersionText,
    globalCommandLine,
    globalFill,
    globalRops, globalRopsAdd,
    globalCops, globalCopset,
    globalInfix,
    globalAbort,
    global',

    -- * Getting global parameter
    GetGlobal (..),
    ropGlobal,
    ropCopset,

    -- * Operator set
    OpSet' (..),
    opset, opsetFill,
  ) where

import qualified Data.Version                        as Ver
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Data                as C
import qualified Koshucode.Baala.Core.Relmap.Option  as C
import qualified Koshucode.Baala.Core.Relmap.Rop     as C
import qualified Koshucode.Baala.Core.Relmap.Result  as C


-- ----------------------  GetGlobal

class GetGlobal h where
    getGlobal :: h c -> Global' h c

ropGlobal :: (GetGlobal h) => C.Intmed' h c -> Global' h c
ropGlobal = getGlobal . C.medHook

-- | Get operator set from 'Intmed'.
ropCopset :: (GetGlobal h) => C.Intmed' h c -> C.CopSet c
ropCopset = globalCopset . ropGlobal


-- ----------------------  Global

-- | Global parameters
data Global' h c = Global
      { globalSynopsis     :: String                -- ^ One-line description of calculator
      , globalVersion      :: Ver.Version           -- ^ Version of calculator
      , globalOpset        :: OpSet' h c            -- ^ Set of operators
      , globalProgram      :: String                -- ^ Name of invoked program
      , globalArgs         :: [String]              -- ^ Command line arguments
      , globalProxy        :: [B.HttpProxy]         -- ^ Proxy setting from environment variables
      , globalTime         :: B.Time                -- ^ Invocation time
      , globalResult       :: C.Result c            -- ^ Result template
      , globalOption       :: C.Option c            -- ^ Options
      , globalSourceCount  :: Int                   -- ^ Sequential number for sources
      , globalSources      :: [B.CodePiece]         -- ^ Included sources
      , globalHook         :: h c                   -- ^ Usually, data resource is used as hook
      }

instance Show (Global' h c) where
    show Global { globalVersion = ver }
        = "Global (" ++ show ver ++ ")"

globalVersionText :: Global' h c -> String
globalVersionText = Ver.showVersion . globalVersion
  
globalCommandLine :: Global' h c -> [String]
globalCommandLine Global { globalProgram = prog, globalArgs = args }
    = prog : args

globalFill :: (C.CContent c) => B.Map (Global' h c)
globalFill g = g

globalRops   :: Global' h c -> [C.Rop' h c]
globalRops    = opsetRopList . globalOpset

globalRopsAdd :: [C.Rop' h c] -> B.Map (Global' h c)
globalRopsAdd rops g = g { globalOpset = opsetFill ops' } where
    ops' = opsetRopsAdd rops ops
    ops  = globalOpset g

globalCops   :: Global' h c -> [C.Cop c]
globalCops    = C.copsetCopList . opsetCop . globalOpset

globalInfix  :: Global' h c -> [B.Named B.InfixHeight]
globalInfix   = C.copsetInfixList . opsetCop . globalOpset

globalCopset :: Global' h c -> C.CopSet c
globalCopset  = opsetCop . globalOpset

globalAbort :: Global' h c -> B.AbortReason -> IO a
globalAbort = B.abort . globalCommandLine

-- | Empty global parameters.
global' :: (C.CBool c, C.CText c) => h c -> Global' h c
global' h = Global
    { globalSynopsis     = "koshu"
    , globalVersion      = Ver.Version [] []
    , globalOpset        = opset
    , globalProgram      = ""
    , globalArgs         = []
    , globalProxy        = []
    , globalTime         = B.timeFromMjd 0
    , globalResult       = C.resultEmpty
    , globalOption       = C.option
    , globalSourceCount  = 0
    , globalSources      = []
    , globalHook         = h }


-- ----------------------  Operator set

-- | Set of relmap and content operators.
data OpSet' h c = OpSet
    { opsetRopList     :: [C.Rop' h c]
    , opsetFindRop     :: C.FindRop' h c
    , opsetCop         :: C.CopSet c
    }

-- | Empty operator set.
opset :: OpSet' h c
opset = OpSet [] find C.copset where
    find _ = Nothing

opsetFill :: B.Map (OpSet' h c)
opsetFill ops = ops2 where
    ops2      = ops { opsetFindRop = findRop
                    , opsetCop     = copset2 }
    copset2   = C.copsetFill $ opsetCop ops
    findRop   = B.assocFinder rops
    rops      = map name $ opsetRopList ops
    name rop  = (B.name rop, rop)

opsetRopsAdd :: [C.Rop' h c] -> B.Map (OpSet' h c)
opsetRopsAdd rops ops = ops { opsetRopList = rops ++ opsetRopList ops }
