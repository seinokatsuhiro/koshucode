{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

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
    global',

    -- * Getting global parameter
    GetGlobal (..),
    ropGlobal,
    ropCopset,

    -- * Operator set
    OpSet' (..),
    opset, opsetFill,

    -- * Option
    Option,
    OptionContent (..),
    option,
    optionBool,
    optionUpdate,
  ) where

import qualified Data.Map.Strict                     as Map
import qualified Data.Version                        as Ver
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Core.Content        as C
import qualified Koshucode.Baala.Core.Relmap.Rop     as C
import qualified Koshucode.Baala.Core.Message        as Msg


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
      , globalOption       :: Option c              -- ^ Options
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
    , globalOption       = option
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


-- ----------------------  Options

type Option c = Map.Map String (OptionContent c)

data OptionContent c
    = OptionBool [c] Bool
    | OptionChar [c] Char
      deriving (Show, Eq, Ord)

option :: (C.CBool c, C.CText c) => Option c
option =
    Map.fromList
           [ ("order"    , OptionBool [C.pBool True, C.pBool False] False)
           , ("sep-char" , OptionChar [C.pText ":", C.pText "|"] ':') ]

optionBool :: String -> Option c -> Bool
optionBool name opt =
    case Map.lookup name opt of
      Just (OptionBool _ b) -> b
      _                     -> B.bug "unknown option"

optionUpdate :: (Eq c, C.CBool c, C.CText c) => Option c -> (String, c) -> B.Ab (Option c)
optionUpdate opt (name, c) = opt' where
    opt' = case Map.lookup name opt of
             Just oc  -> upd oc
             Nothing  -> Msg.adlib "unknown option"

    upd (OptionBool cs _) | c `elem` cs = ins (OptionBool cs $ C.gBool c)
    upd (OptionChar cs _) | c `elem` cs = ins (OptionChar cs $ head $ C.gText c)
    upd _ = Msg.adlib "unknown content"

    ins oc = Right $ Map.insert name oc opt

