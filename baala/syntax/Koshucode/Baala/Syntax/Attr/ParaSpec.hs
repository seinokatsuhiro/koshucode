{-# OPTIONS_GHC -Wall #-}

-- | Parameter specification.

module Koshucode.Baala.Syntax.Attr.ParaSpec
  ( ParaSpec (..), ParaSpecPos (..), 
    paraSpec,
    paraJust, paraMin, paraMax, paraRange,
    paraReq, paraOpt, paraMult,
  ) where

import qualified Koshucode.Baala.Base          as B

-- | Parameter specification.
data ParaSpec n
    = ParaSpec
      { paraSpecPos   :: ParaSpecPos   -- ^ Positional parameter specification
      , paraSpecReq   :: [n]           -- ^ Required parameter specification
      , paraSpecOpt   :: [n]           -- ^ Optional parameter specification
      , paraSpecMult  :: [n]
      } deriving (Show, Eq, Ord)

-- | Positional parameter specification.
data ParaSpecPos
    = ParaPosJust  Int      -- ^ Required length of positional parameter
    | ParaPosMin   Int      -- ^ Lower bound of parameter length
    | ParaPosMax   Int      -- ^ Upper bound of parameter length
    | ParaPosRange Int Int  -- ^ Lower and upper bound of parameter length
      deriving (Show, Eq, Ord)

-- | Empty parameter specification.
paraSpec :: ParaSpec n
paraSpec = ParaSpec (ParaPosJust 0) [] [] []

paraJust, paraMin, paraMax :: (Show n, Ord n) => ParaSpec n -> Int -> ParaSpec n
paraJust  ty n  = paraCheck $ ty { paraSpecPos = ParaPosJust n }
paraMin   ty n  = paraCheck $ ty { paraSpecPos = ParaPosMin  n }
paraMax   ty n  = paraCheck $ ty { paraSpecPos = ParaPosMax  n }

paraRange :: (Show n, Ord n) => ParaSpec n -> (Int, Int) -> ParaSpec n
paraRange ty (m, n) = paraCheck $ ty { paraSpecPos = ParaPosRange m n }

paraReq, paraOpt, paraMult :: (Show n, Ord n) => ParaSpec n -> [n] -> ParaSpec n
paraReq  ty ns  = paraCheck $ ty { paraSpecReq  = ns }
paraOpt  ty ns  = paraCheck $ ty { paraSpecOpt  = ns }
paraMult ty ns  = paraCheck $ ty { paraSpecMult = ns }

paraCheck :: (Show n, Ord n) => B.Map (ParaSpec n)
paraCheck ty@(ParaSpec _ req opt mul)
    | null dup   = ty
    | otherwise  = B.bug $ "duplicate para names: " ++ show dup
    where ns     = req ++ opt ++ mul
          dup    = B.duplicates ns

