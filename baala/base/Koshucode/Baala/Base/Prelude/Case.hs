{-# OPTIONS_GHC -Wall #-}

-- | Case-error versions of functions.

module Koshucode.Baala.Base.Prelude.Case
  ( -- * Null
    caseNull,
    headNull, tailNull,
    minimumNull, maximumNull,

    -- * Zero
    caseZero,
    quotZero, remZero,
    divZero, modZero,
    quotRemZero, divModZero,
  ) where


-- --------------------------------------------  Null

-- | Apply function unless null list.
caseNull :: (Foldable t) => (t a -> b) -> b -> t a -> b
caseNull f x xs | null xs   = x
                | otherwise = f xs

-- | Case-null version of 'head'.
--
--   >>> headNull 0 [20, 70, 80] :: Int
--   20
--
--   >>> headNull 0 [] :: Int
--   0

headNull :: a -> [a] -> a
headNull = caseNull head

-- | Case-null version of 'tail'.
tailNull :: [a] -> [a] -> [a]
tailNull = caseNull tail

-- | Case-null version of 'minimum'.
--
--   >>> minimumNull 100 [20, 70, 80] :: Int
--   20
--
--   >>> minimumNull 100 [] :: Int
--   100

minimumNull :: (Ord a) => a -> [a] -> a
minimumNull = caseNull minimum

-- | Case-null version of 'maximum'.
maximumNull :: (Ord a) => a -> [a] -> a
maximumNull = caseNull maximum


-- --------------------------------------------  Zero

-- | Apply function unless 0.
caseZero :: (Eq a, Num a) => (a -> b) -> b -> a -> b
caseZero f y x | x == 0     = y
               | otherwise  = f x

caseZeroBy :: (Eq a, Num a) => (a -> a -> b) -> b -> a -> a -> b
caseZeroBy f y n = caseZero (f n) y

-- | Case-zero version of 'quot'.
quotZero :: (Integral a) => a -> a -> a -> a
quotZero = caseZeroBy quot

-- | Case-zero version of 'rem'.
remZero :: (Integral a) => a -> a -> a -> a
remZero = caseZeroBy rem

-- | Case-zero version of 'div'.
--
--   >>> divZero 0 10 2 :: Int
--   5
--
--   >>> divZero 0 10 0 :: Int
--   0

divZero :: (Integral a) => a -> a -> a -> a
divZero = caseZeroBy div

-- | Case-zero version of 'mod'.
modZero :: (Integral a) => a -> a -> a -> a
modZero = caseZeroBy mod

-- | Case-zero version of 'quotRem'.
--
--   >>> quotRemZero (0,0) 20 6 :: (Int, Int)
--   (3,2)
--
--   >>> quotRemZero (0,0) 20 0 :: (Int, Int)
--   (0,0)
--
quotRemZero :: (Integral a) => (a, a) -> a -> a -> (a, a)
quotRemZero = caseZeroBy quotRem

-- | Case-zero version of 'divMod'.
divModZero :: (Integral a) => (a, a) -> a -> a -> (a, a)
divModZero = caseZeroBy divMod

