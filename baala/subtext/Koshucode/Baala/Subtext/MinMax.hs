{-# OPTIONS_GHC -Wall #-}

-- | Lower and upper bound.

module Koshucode.Baala.Subtext.MinMax
 ( MinMax (..),
   atLeast, atMost,
   times,
 ) where

-- | Lower and upper bound.
data MinMax
  = Min    Int            -- ^ Lower bound and unlimited upper bound
  | MinMax Int Int        -- ^ Lower and upper bound
    deriving (Show, Eq, Ord)

-- | Lower bound.
lowerBound :: MinMax -> Int
lowerBound (Min n)      = n
lowerBound (MinMax n _) = n

-- | Test lower bound.
--
--   >>> atLeast 1 $ Min 0
--   False
--
--   >>> atLeast 1 $ Min 1
--   True

atLeast :: Int -> MinMax -> Bool
atLeast b m = (lowerBound m) >= b

-- | Test upper bound.
atMost :: Int -> MinMax -> Bool
atMost _ (Min _)      = False
atMost b (MinMax _ n) = n <= b

-- | Compose nested min-max.
--
--   >>> MinMax 2 4 `times` MinMax 3 5
--   MinMax 6 20
--
--   >>> MinMax 2 4 `times` Min 3
--   Min 6

times :: MinMax -> MinMax -> MinMax
times (MinMax a b) (MinMax x y)  = MinMax (a * x) (b * y)
times (Min    a)   (Min    x)    = Min    (a * x)
times (MinMax a _) (Min    x)    = Min    (a * x)
times (Min    a)   (MinMax x _)  = Min    (a * x)
