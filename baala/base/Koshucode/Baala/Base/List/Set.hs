{-# OPTIONS_GHC -Wall #-}

-- | Set-like operation.

module Koshucode.Baala.Base.List.Set
  ( duplicates, duplicated,
    unique,
    uniqueConcat,
    unionUp,
    keepMember, omitMember, sublist,
    setList, setEq,
    disjoint, overlap,
  ) where

import qualified Data.List                      as List
import qualified Data.Set                       as Set
import qualified Koshucode.Baala.Overture       as O


-- | Extract duplicate elements.
--
--   >>> duplicates "banana"
--   "ana"
--
--   >>> duplicates "grape"
--   ""
--
duplicates :: (Ord a) => [a] -> [a]
duplicates xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = x : loop xs2 set
        | otherwise        = loop xs2 (Set.insert x set)

-- | Test list has duplicated elements.
--
--   >>> duplicated "banana"
--   True
--
--   >>> duplicated "grape"
--   False
--
duplicated :: (Ord a) => O.Test [a]
duplicated = not . null . duplicates

-- | Remove duplicate elements.
--
--   >>> unique "banana"
--   "ban"
--
unique :: (Ord a) => [a] -> [a]
unique xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = loop xs2 set
        | otherwise        = x : loop xs2 (Set.insert x set)

-- | Collect elements in occurence order.
--
--   >>> uniqueConcat ["apple", "banana", "cocoa"]
--   "aplebnco"
--
uniqueConcat :: (Ord a) => [[a]] -> [a]
uniqueConcat = unique . concat

-- | Union list to base list.
--
--   >>> unionUp "cde" "abc"
--   "deabc"
--
--   >>> List.union "cde" "abc"
--   "cdeab"
--
unionUp :: (Eq a) => [a] -> [a] -> [a]
unionUp xs ys = (xs List.\\ ys) ++ ys

-- | Filter by set membership.
--   @keepMember@ /A/ /B/ is same to the intersection of /A/ and /B/,
--   except for the ordering of /B/ is preserved.
--
--   >>> keepMember "cba" "abcdefg"
--   "abc"
--
--   >>> keepMember "abcdefg" "cba"
--   "cba"
--
{-# DEPRECATED keepMember "Use 'snipShare' instead." #-}
keepMember :: (Ord a) => [a] -> [a] -> [a]
keepMember xs = filter (`Set.member` Set.fromList xs)

-- | Anti-filter by set membership.
--   @omitMember@ /A/ /B/ is same to /B/ minus /f/A,
--   except for the ordering of /B/ is preserved.
--
--   >>> omitMember "cba" "abcdefg"
--   "defg"
--
--   >>> omitMember "abcdefg" "cba"
--   ""
--
{-# DEPRECATED omitMember "Use 'snipRight' instead." #-}
omitMember :: (Ord a) => [a] -> [a] -> [a]
omitMember xs = filter (`Set.notMember` Set.fromList xs)

-- | Test sublist.
--
--   >>> sublist "cab" "abcdefg"
--   True
--
--   >>> sublist "cab" "dec"
--   False
--
sublist :: (Ord a) => [a] -> [a] -> Bool
sublist xs ys = null $ omitMember ys xs

-- | Convert to set-like list, in other words,
--   remove duplicate elements and sort list.
--
--   >>> setList "abracadabra"
--   "abcdr"
--
setList :: (Ord a) => [a] -> [a]
setList = Set.toAscList . Set.fromList

-- | Set-like equality, in other words, 
--   duplication and ordering are ignored.
--
--   >>> setEq "abc" "bca"
--   True
--
--   >>> setEq "abc" "ab"
--   False
--
setEq :: (Ord a) => [a] -> [a] -> Bool
setEq xs ys = (Set.fromList xs) == (Set.fromList ys)

-- | Test two list has no elements in common.
--
--   >>> disjoint "abc" "de"
--   True
--
--   >>> disjoint "abc" "cde"
--   False
--
disjoint :: (Eq a) => [a] -> [a] -> Bool
disjoint a b = null (a `List.intersect` b)

-- | Test two list has some common elements.
--
--   >>> overlap "abc" "de"
--   False
--
--   >>> overlap "abc" "cde"
--   True
--
overlap :: (Eq a) => [a] -> [a] -> Bool
overlap a b = not $ disjoint a b

