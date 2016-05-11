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
    opsetFill,

    -- * Feature
    Feature (..),
  ) where

import qualified Data.Version                        as Ver
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Data                as D
import qualified Koshucode.Baala.Core.Relmap.Option  as C
import qualified Koshucode.Baala.Core.Relmap.Rop     as C
import qualified Koshucode.Baala.Core.Relmap.Result  as C


-- ----------------------  GetGlobal

class GetGlobal h where
    getGlobal :: h c -> Global' h c

ropGlobal :: (GetGlobal h) => C.Intmed' h c -> Global' h c
ropGlobal = getGlobal . C.medHook

-- | Get operator set from 'Intmed'.
ropCopset :: (GetGlobal h) => C.Intmed' h c -> D.CopSet c
ropCopset = globalCopset . ropGlobal


-- ----------------------  Global

-- | Global parameters
data Global' h c = Global
      { globalSynopsis     :: String                -- ^ One-line description of calculator
      , globalVersion      :: Ver.Version           -- ^ Version of calculator
      , globalOpset        :: OpSet' h c            -- ^ Set of operators
      , globalFeature      :: Feature               -- ^ Features
      , globalProgram      :: String                -- ^ Name of invoked program
      , globalArgs         :: [String]              -- ^ Command line arguments
      , globalProxy        :: [B.HttpProxy]         -- ^ Proxy setting from environment variables
      , globalTime         :: D.Time                -- ^ Invocation time
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

globalFill :: (D.CContent c) => B.Map (Global' h c)
globalFill g = g

globalRops   :: Global' h c -> [C.Rop' h c]
globalRops    = opsetRopList . globalOpset

globalRopsAdd :: [C.Rop' h c] -> B.Map (Global' h c)
globalRopsAdd rops g = g { globalOpset = opsetFill ops' } where
    ops' = opsetRopsAdd rops ops
    ops  = globalOpset g

globalCops   :: Global' h c -> [D.Cop c]
globalCops    = D.copsetCopList . opsetCop . globalOpset

globalInfix  :: Global' h c -> [B.Named B.InfixHeight]
globalInfix   = D.copsetInfixList . opsetCop . globalOpset

globalCopset :: Global' h c -> D.CopSet c
globalCopset  = opsetCop . globalOpset

globalAbort :: Global' h c -> B.AbortReason -> IO a
globalAbort = B.abort . globalCommandLine

-- | Empty global parameters.
global' :: (D.CBool c, D.CText c) => h c -> Global' h c
global' h = Global
    { globalSynopsis     = "koshu"
    , globalVersion      = Ver.Version [] []
    , globalOpset        = B.def
    , globalFeature      = B.def
    , globalProgram      = ""
    , globalArgs         = []
    , globalProxy        = []
    , globalTime         = D.timeFromMjd 0
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
    , opsetCop         :: D.CopSet c
    }

-- | Empty operator set.
instance B.Default (OpSet' h c) where
    def = OpSet { opsetRopList = []
                , opsetFindRop = const Nothing
                , opsetCop     = D.copset }

opsetFill :: B.Map (OpSet' h c)
opsetFill ops = ops2 where
    ops2      = ops { opsetFindRop = findRop
                    , opsetCop     = copset2 }
    copset2   = D.copsetFill $ opsetCop ops
    findRop   = B.assocFinder rops
    rops      = map name $ opsetRopList ops
    name rop  = (B.name rop, rop)

opsetRopsAdd :: [C.Rop' h c] -> B.Map (OpSet' h c)
opsetRopsAdd rops ops = ops { opsetRopList = rops ++ opsetRopList ops }


-- ----------------------  Feature

-- | Enable or disable featrues.
data Feature = Feature {
      featInputClause    :: Bool    -- ^ Enable @input@ clause
    , featOutputClause   :: Bool    -- ^ Enable @output@ clause
    } deriving (Show, Eq, Ord)

instance B.Default Feature where
    def = Feature { featInputClause  = True
                  , featOutputClause = True
                  }

