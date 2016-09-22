{-# OPTIONS_GHC -Wall #-}

-- | Lower and upper bound.

module Koshucode.Baala.Subtext.MinMax
 ( MinMax (..),
   atLeast, atMost,
 ) where

-- | Lower and upper bound.
data MinMax
  = Min    Int            -- ^ Lower bound
  | MinMax Int Int        -- ^ Lower and upper bound
    deriving (Show, Eq, Ord)

-- | Test lower bound.
atLeast :: Int -> MinMax -> Bool
atLeast b (Min n)      = n >= b
atLeast b (MinMax n _) = n >= b

-- | Test upper bound.
atMost :: Int -> MinMax -> Bool
atMost _ (Min _)      = False
atMost b (MinMax _ n) = n <= b

