{-# OPTIONS_GHC -Wall #-}

-- | Global parameters.

module Koshucode.Baala.Core.Relmap.Global
  ( -- * Global parameter
    Global' (..),
    globalVersionText,
    globalCommandLine,
    globalFill,
    globalRops, globalRopsAdd,
    globalCops,
    globalInfix,
    globalAbort,
    global',

    -- * Getting global parameter
    GetGlobal' (..),
    GetGlobal (..),

    -- * Operator set
    OpSet' (..),
    opsetFill,

    -- * Feature
    Feature (..),
  ) where

import qualified Data.Version                        as Ver
import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Type                as T
import qualified Koshucode.Baala.Data                as D
import qualified Koshucode.Baala.Core.Relmap.Option  as C
import qualified Koshucode.Baala.Core.Relmap.Rop     as C
import qualified Koshucode.Baala.Core.Relmap.Result  as C


-- ----------------------  Global

-- | Global parameters
data Global' h c = Global
      { globalSynopsis     :: String            -- ^ One-line description of calculator
      , globalVersion      :: Ver.Version       -- ^ Version of calculator
      , globalOpset        :: OpSet' h c        -- ^ Set of operators
      , globalFeature      :: Feature           -- ^ Features
      , globalProgram      :: String            -- ^ Name of invoked program
      , globalArgs         :: [String]          -- ^ Command line arguments
      , globalProxy        :: [B.HttpProxy]     -- ^ Proxy setting from environment variables
      , globalTime         :: T.Time            -- ^ Invocation time
      , globalResult       :: C.Result c        -- ^ Result template
      , globalOption       :: C.Option c        -- ^ Options
      , globalSourceLimit  :: Int               -- ^ Limit number of including sources
      , globalSourceCount  :: Int               -- ^ Sequential number for sources
      , globalSources      :: [B.IxIOPoint]     -- ^ Included sources
      , globalHook         :: h c               -- ^ Usually, data resource is used as hook
      }

instance Show (Global' h c) where
    show Global { globalVersion = ver }
        = "Global (" ++ show ver ++ ")"

-- | Textual representation of version number.
globalVersionText :: Global' h c -> String
globalVersionText = Ver.showVersion . globalVersion

-- | Command line text, i.e., program name and arguments.
globalCommandLine :: Global' h c -> [String]
globalCommandLine Global { globalProgram = prog, globalArgs = args }
    = prog : args

-- | Make complete global parameter by filling auto-generated part.
globalFill :: (D.CContent c) => O.Map (Global' h c)
globalFill g = g

-- | List of relmap operators.
globalRops :: Global' h c -> [C.Rop' h c]
globalRops = opsetRopList . globalOpset

-- | Add relmap operators.
globalRopsAdd :: [C.Rop' h c] -> O.Map (Global' h c)
globalRopsAdd rops g = g { globalOpset = opsetFill ops' } where
    ops' = opsetRopsAdd rops ops
    ops  = globalOpset g

-- | List of content operators.
globalCops :: Global' h c -> [D.Cop c]
globalCops = D.copsetCopList . D.getCops

-- | List of settings of infix operators.
globalInfix :: Global' h c -> [(S.Chars, B.InfixHeight)]
globalInfix = D.copsetInfixList . opsetCop . globalOpset

-- | Abort with command line.
globalAbort :: Global' h c -> B.AbortReason -> IO a
globalAbort = B.abortCommand . globalCommandLine

-- | Empty global parameters.
global' :: (Show c, D.CBool c, D.CText c) => h c -> Global' h c
global' h = Global
    { globalSynopsis     = "koshu"
    , globalVersion      = Ver.Version [] []
    , globalOpset        = B.def
    , globalFeature      = B.def
    , globalProgram      = ""
    , globalArgs         = []
    , globalProxy        = []
    , globalTime         = B.def
    , globalResult       = B.def
    , globalOption       = C.option
    , globalSourceLimit  = 1000
    , globalSourceCount  = 0
    , globalSources      = []
    , globalHook         = h }


-- ----------------------  Getting global parameter

-- | Type which has global parameter.
class GetGlobal' h where
    -- | Get global parameter.
    getGlobal' :: h c -> Global' h c

-- | Type which has global parameter.
class GetGlobal a where
    -- | Get global parameter.
    getGlobal :: (GetGlobal' h) => a h c -> Global' h c

-- | Get global parameter from relmap intermediate data.
instance GetGlobal C.Intmed' where
    getGlobal = getGlobal' . C.medHook

instance D.GetCops (Global' h) where
    getCops = opsetCop . globalOpset


-- ----------------------  Operator set

-- | Set of relmap and content operators.
data OpSet' h c = OpSet
    { opsetRopList  :: [C.Rop' h c]     -- ^ Relmap operators.
    , opsetFindRop  :: C.FindRop' h c   -- ^ Operator finder.
    , opsetCop      :: D.CopSet c       -- ^ Content operators.
    }

-- | Empty operator set.
instance B.Default (OpSet' h c) where
    def = OpSet { opsetRopList = []
                , opsetFindRop = O.nothing
                , opsetCop     = B.def }

-- | Make complete operator set by filling auto-generated part.
opsetFill :: O.Map (OpSet' h c)
opsetFill ops = ops2 where
    ops2      = ops { opsetFindRop = findRop
                    , opsetCop     = copset2 }
    copset2   = D.copsetFill $ opsetCop ops
    findRop   = B.assocFinder rops
    rops      = map name $ opsetRopList ops
    name rop  = (B.name rop, rop)

opsetRopsAdd :: [C.Rop' h c] -> O.Map (OpSet' h c)
opsetRopsAdd rops ops = ops { opsetRopList = rops ++ opsetRopList ops }


-- ----------------------  Feature

-- | Enable or disable featrues.
data Feature = Feature {
      featInputClause    :: Bool    -- ^ Enable @input@ clause
    , featOutputClause   :: Bool    -- ^ Enable @output@ clause
    , featAutoOutput     :: Bool    -- ^ Automatic output when no assertions.
    } deriving (Show, Eq, Ord)

instance B.Default Feature where
    def = Feature { featInputClause  = True
                  , featOutputClause = True
                  , featAutoOutput   = False
                  }

