{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parameter specification.

module Koshucode.Baala.Syntax.Attr.ParaSpec
  ( -- * Specification
    ParaSpec (..), ParaSpecPos (..), 

    -- * Construction
    -- ** Positional parameter
    paraJust, paraMin, paraMax, paraRange,
    -- ** Named parameter
    paraReq, paraOpt, paraMult,

    -- * Unmatch reason
    ParaUnmatch (..),
    paraUnmatch, paraMatch, 
    paraSelect, 
  ) where

import qualified Data.Map.Strict                   as Map
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax.Attr.Para  as S


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
    = ParaPosJust  Int      -- ^ Required length of positional parameter
    | ParaPosMin   Int      -- ^ Lower bound of parameter length
    | ParaPosMax   Int      -- ^ Upper bound of parameter length
    | ParaPosRange Int Int  -- ^ Lower and upper bound of parameter length
      deriving (Show, Eq, Ord)

-- | No parameters
instance B.Default (ParaSpec n) where
    def = ParaSpec { paraSpecPos  = ParaPosJust 0
                   , paraSpecReq  = []
                   , paraSpecOpt  = []
                   , paraSpecMult = [] }


-- --------------------------------------------  Construct

paraCheck :: (Show n, Ord n) => B.Map (ParaSpec n)
paraCheck ty@(ParaSpec _ req opt mul)
    | null dup   = ty
    | otherwise  = B.bug $ "duplicate para names: " ++ show dup
    where ns     = req ++ opt ++ mul
          dup    = B.duplicates ns

-- ----------------------  Positional

-- | Fixed-length positional parameter.
paraJust :: (Show n, Ord n) => ParaSpec n -> Int -> ParaSpec n
-- | Lower bound of length of positional parameter.
paraMin :: (Show n, Ord n) => ParaSpec n -> Int -> ParaSpec n
-- | Upper bound of length of positional parameter.
paraMax :: (Show n, Ord n) => ParaSpec n -> Int -> ParaSpec n
-- | Lower and upper bound of length of positional parameter.
paraRange :: (Show n, Ord n) => ParaSpec n -> (Int, Int) -> ParaSpec n

paraJust  ty n      = paraCheck $ ty { paraSpecPos = ParaPosJust n }
paraMin   ty n      = paraCheck $ ty { paraSpecPos = ParaPosMin  n }
paraMax   ty n      = paraCheck $ ty { paraSpecPos = ParaPosMax  n }
paraRange ty (m, n) = paraCheck $ ty { paraSpecPos = ParaPosRange m n }

-- ----------------------  Named

-- | Required named parameter.
paraReq :: (Show n, Ord n) => ParaSpec n -> [n] -> ParaSpec n
-- | Optional named parameter.
paraOpt :: (Show n, Ord n) => ParaSpec n -> [n] -> ParaSpec n
-- | Multiple-occurence parameter.
paraMult :: (Show n, Ord n) => ParaSpec n -> [n] -> ParaSpec n

paraReq  ty ns  = paraCheck $ ty { paraSpecReq  = ns }
paraOpt  ty ns  = paraCheck $ ty { paraSpecOpt  = ns }
paraMult ty ns  = paraCheck $ ty { paraSpecMult = ns }


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
    match (ParaPosJust c)     | n == c            = Nothing
    match (ParaPosMin  a)     | n >= a            = Nothing
    match (ParaPosMax  b)     | n <= b            = Nothing
    match (ParaPosRange a b)  | n >= a && n <= b  = Nothing
    match p                                       = Just $ ParaOutOfRange n p
    n = length ps

-- | Test parameter satisfies specification.
paraMatch :: (Eq n) => S.Para n a -> ParaSpec n -> Bool
paraMatch p t = paraUnmatch p t == Nothing

paraSelect :: (Eq n) => b -> [(ParaSpec n, S.Para n a -> b)] -> S.Para n a -> b
paraSelect b ps p = loop ps where
    loop [] = b
    loop ((ty, body) : ps2)
        | paraMatch p ty    = body p
        | otherwise         = loop ps2

