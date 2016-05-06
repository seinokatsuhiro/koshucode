{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parameter specification.

module Koshucode.Baala.Syntax.Para.ParaSpec
  ( -- * Specification
    ParaSpec (..), ParaSpecPos (..), 

    -- * Construction
    ParaSpecMap, paraSpec,
    -- ** Positional parameter
    paraMin, paraMax, paraJust, paraRange,
    -- ** Named parameter
    paraReq, paraOpt, paraMult,

    -- * Unmatch reason
    ParaUnmatch (..),
    paraUnmatch, paraMatch, 
    ParaTo,
    paraSelect, 
  ) where

import qualified Data.Map.Strict                   as Map
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax.Para.Para  as S


-- --------------------------------------------  Specification

-- | Parameter specification.
data ParaSpec n
    = ParaSpec
      { paraSpecPos   :: ParaSpecPos   -- ^ Positional parameter
      , paraSpecReq   :: [n]           -- ^ Required parameter
      , paraSpecOpt   :: [n]           -- ^ Optional parameter
      , paraSpecMult  :: [n]           -- ^ Multiple-occurence parameter
      } deriving (Show, Eq, Ord)

-- | Positional parameter specification.
data ParaSpecPos
    = ParaPosMin   Int      -- ^ Lower bound of parameter length
    | ParaPosRange Int Int  -- ^ Lower and upper bound of parameter length
      deriving (Show, Eq, Ord)

-- | No parameters
instance B.Default (ParaSpec n) where
    def = ParaSpec { paraSpecPos  = ParaPosRange 0 0
                   , paraSpecReq  = []
                   , paraSpecOpt  = []
                   , paraSpecMult = [] }


-- --------------------------------------------  Construct

type ParaSpecMap n = B.Map (ParaSpec n)

paraSpec :: (Show n, Ord n) => ParaSpecMap n -> ParaSpec n
paraSpec edit = paraCheck $ edit B.def

paraCheck :: (Show n, Ord n) => ParaSpecMap n
paraCheck spec@(ParaSpec _ req opt mul)
    | null dup   = spec
    | otherwise  = B.bug $ "duplicate para names: " ++ show dup
    where ns     = req ++ opt ++ mul
          dup    = B.duplicates ns

-- ----------------------  Positional

-- | Lower bound of length of positional parameter.
paraMin :: Int -> ParaSpecMap n
-- | Upper bound of length of positional parameter.
paraMax :: Int -> ParaSpecMap n
-- | Fixed-length positional parameter.
paraJust :: Int -> ParaSpecMap n
-- | Lower and upper bound of length of positional parameter.
paraRange :: Int -> Int -> ParaSpecMap n

paraMin   n   = paraPos $ ParaPosMin   n
paraMax   n   = paraPos $ ParaPosRange 0 n
paraJust  n   = paraPos $ ParaPosRange n n
paraRange m n = paraPos $ ParaPosRange m n

paraPos :: ParaSpecPos -> ParaSpecMap n
paraPos pos spec = spec { paraSpecPos = pos }

-- ----------------------  Named

-- | Required named parameter.
paraReq :: [n] -> ParaSpecMap n
-- | Optional named parameter.
paraOpt :: [n] -> ParaSpecMap n
-- | Multiple-occurence parameter.
paraMult :: [n] -> ParaSpecMap n

paraReq  ns spec  = spec { paraSpecReq  = ns }
paraOpt  ns spec  = spec { paraSpecOpt  = ns }
paraMult ns spec  = spec { paraSpecMult = ns }


-- --------------------------------------------  Unmatch

-- | Unmatch reason of real parameter and its specifition.
data ParaUnmatch n
    = ParaOutOfRange Int ParaSpecPos -- ^ Positional parameter is unmatched.
    | ParaUnknown  [n]    -- ^ Unknown parameter is specified.
    | ParaMissing  [n]    -- ^ Required parameter is missing.
    | ParaMultiple [n]    -- ^ Parameter occurs more than once.
      deriving (Show, Eq, Ord)

-- | Create unmatch reason when parameter does not satisfies specification.
paraUnmatch :: forall n a. (Eq n) => S.Para n a -> ParaSpec n -> Maybe (ParaUnmatch n)
paraUnmatch p (ParaSpec pos req opt mul)
    | upos /= Nothing   = upos
    | unknowns  /= []   = Just $ ParaUnknown  unknowns
    | missings  /= []   = Just $ ParaMissing  missings
    | multiples /= []   = Just $ ParaMultiple multiples
    | otherwise         = Nothing
    where
      upos              :: Maybe (ParaUnmatch n)
      upos              = paraPosUnmatch (S.paraPos p) pos
      ns                = Map.keys $ S.paraName p
      ns2               = S.paraMultipleNames p
      total             = req ++ opt ++ mul
      unknowns          = ns  B.\\ total
      missings          = req B.\\ ns
      multiples         = ns2 B.\\ mul

paraPosUnmatch :: [a] -> ParaSpecPos -> Maybe (ParaUnmatch n)
paraPosUnmatch ps = match where
    match (ParaPosMin  a)     | n >= a            = Nothing
    match (ParaPosRange a b)  | n >= a && n <= b  = Nothing
    match p                                       = Just $ ParaOutOfRange n p
    n = length ps

-- | Test parameter satisfies specification.
paraMatch :: (Eq n) => S.Para n a -> ParaSpec n -> Bool
paraMatch p spec = paraUnmatch p spec == Nothing

-- | Map parameter to some value.
type ParaTo n a b = S.Para n a -> b

-- | Select matched specification and apply 'ParaTo' function.
paraSelect :: (Eq n) => b -> [(ParaSpec n, ParaTo n a b)] -> ParaTo n a b
paraSelect b ps p = loop ps where
    loop [] = b
    loop ((spec, paraTo) : ps2)
        | paraMatch p spec    = paraTo p
        | otherwise           = loop ps2

