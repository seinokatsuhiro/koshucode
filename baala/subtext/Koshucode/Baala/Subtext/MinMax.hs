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

-- | Lower bound.
lowerBound :: MinMax -> Int
lowerBound (Min n)      = n
lowerBound (MinMax n _) = n

-- | Test lower bound.
atLeast :: Int -> MinMax -> Bool
atLeast b m = (lowerBound m) >= b

-- | Test upper bound.
atMost :: Int -> MinMax -> Bool
atMost _ (Min _)      = False
atMost b (MinMax _ n) = n <= b

