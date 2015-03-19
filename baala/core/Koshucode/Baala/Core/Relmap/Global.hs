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
    optionParse,
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
    = OptionBool Bool
    | OptionChar [Char] Char
    | OptionTerms [String]
      deriving (Show, Eq, Ord)

option :: (C.CBool c, C.CText c) => Option c
option =
    Map.fromList
           [ ("order"    , OptionBool False)
           , ("sep-char" , OptionChar ":|" ':')
           , ("forward"  , OptionTerms [])
           , ("backward" , OptionTerms []) ]

optionBool :: String -> Option c -> Bool
optionBool name opt =
    case Map.lookup name opt of
      Just (OptionBool b) -> b
      _                   -> B.bug "unknown option"

optionParse :: (Eq c, C.CBool c, C.CText c)
  => C.CalcContent c -> [B.Token] -> B.AbMap (Option c)
optionParse calc toks opt =
    do assn  <- optionAssn toks
       B.foldM (optionUpdate calc) opt assn

type NamedT a = ((String, [B.TTree]), a)

optionAssn :: [B.Token] -> B.Ab [NamedT [B.TTree]]
optionAssn toks =
    do trees <- B.tokenTrees toks
       case B.assocBy maybeName trees of
         ([], assoc) -> Right assoc
         _           -> Msg.adlib "extra input"
    where
      maybeName pt@(B.TextLeafRaw _ n) = Just (n, [pt])
      maybeName _ = Nothing

optionUpdate :: (Eq c, C.CBool c, C.CText c)
   => C.CalcContent c -> Option c -> NamedT [B.TTree] -> B.Ab (Option c)
optionUpdate calc opt ((name, pt), trees) =
    Msg.abOption pt $ do
      case Map.lookup name opt of
        Just oc  -> Msg.abOption trees $ upd oc
        Nothing  -> Msg.adlib $ "unknown option: " ++ name
    where
      abc = calc $ B.wrapTrees trees

      upd (OptionBool    _) = do let ab = C.getBool abc
                                 bool <- ab
                                 ins $ OptionBool bool

      upd (OptionChar cs _) = do let ab = C.getText abc
                                 text <- ab
                                 case text of
                                   [ch] | elem ch cs -> ins $ OptionChar cs ch
                                   _                 -> Msg.adlib "not one letter"

      upd (OptionTerms _)   = do terms <- mapM C.treeToFlatTerm trees
                                 ins $ OptionTerms terms
                                                        
      ins oc = Right $ Map.insert name oc opt

