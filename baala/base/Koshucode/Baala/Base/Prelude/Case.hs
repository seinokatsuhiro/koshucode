{-# OPTIONS_GHC -Wall #-}

-- | Case-error versions of functions.

module Koshucode.Baala.Base.Prelude.Case
  ( -- * Null
    caseNull,
    headNull, tailNull,
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

