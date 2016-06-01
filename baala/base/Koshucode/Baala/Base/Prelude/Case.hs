{-# OPTIONS_GHC -Wall #-}

-- | Case-error versions of functions.

module Koshucode.Baala.Base.Prelude.Case
  ( caseNull,
    minimumNull, maximumNull
  ) where

-- | Apply function unless null list.
caseNull :: (Foldable t) => (t a -> b) -> b -> t a -> b
caseNull f x xs | null xs   = x
                | otherwise = f xs

-- | Case-null version of 'minimum'.
minimumNull :: (Ord a) => a -> [a] -> a
minimumNull = caseNull minimum

-- | Case-null version of 'maximum'.
maximumNull :: (Ord a) => a -> [a] -> a
maximumNull = caseNull maximum

