{-# OPTIONS_GHC -Wall #-}

-- | Select elements.

module Koshucode.Baala.Base.List.Select
  ( -- * Type
    Select, Select2,
  
    -- * Index
    selectIndex, selectIndexFull, selectIndexBoth,
  
    -- * Selection
    selectElems, selectOthers, selectBoth,

    -- * Trisection
    selectShare, selectLeft, selectRight,

    -- * Permutaion
    permuteForward, permuteBackward, 
    permuteForward2, permuteBackward2,
    permuteOrder,
  ) where

import qualified Data.List                           as List
import qualified Data.Set                            as Set
import qualified Koshucode.Baala.Overture            as O


-- --------------------------------------------  Type

-- | Select elements using indicies.
type Select a = [Int] -> O.Map [a]

-- | Selection for different types.
type Select2 a b = (Select a, Select b)


-- --------------------------------------------  Index

-- | Indices of shared elements.
--
--   >>> selectIndex "bdk" "abcdefg"
--   [1,3]
--
selectIndex :: (Eq a) => [a] -> [a] -> [Int]
selectIndex ks xs = filter (>= 0) $ selectIndexFull ks xs

-- | Indicies of shared-and-unknown elements.
--
--   >>> selectIndexFull "bcx" "abcdefg"
--   [1,2,-1]
--
selectIndexFull :: (Eq a) => [a] -> [a] -> [Int]
selectIndexFull ks xs = map ind ks where
    ind k = maybe (-1) id $ List.elemIndex k xs

-- | Left-and-right indices of shared elements.
--
--   >>> selectIndexBoth "abc" "bcd"
--   ([1,2], [0,1])
--
selectIndexBoth :: (Ord a) => [a] -> [a] -> ([Int], [Int])
selectIndexBoth xs1 xs2 =
    let sh = selectShare xs1 xs2
    in (selectIndex sh xs1, selectIndex sh xs2)


-- --------------------------------------------  Selection

-- | Pick up indexed elements.
--
--   >>> [1,3] `selectElems` "abcdefg"
--   "bd"
--
--   >>> [1,3] `selectElems` "ABCDEFG"
--   "BD"
--
selectElems :: Select a
selectElems ps xs = loop ps xs 0 where
    loop pps@(p:ps2) (x:xs2) i =
        case compare p i of
          EQ             -> x : loop ps2 xs2 (i + 1)
          GT             ->     loop pps xs2 (i + 1)
          LT | p >= 0    -> selectElems pps xs  -- restart
             | otherwise -> selectElems ps2 xs  -- ignore minus index
    loop pps@(p:ps2) [] i
        | p < i     = selectElems pps xs  -- restart
        | otherwise = selectElems ps2 xs  -- ignore large index
    loop [] _ _ = []

-- Simple implementation
-- selectElems :: [Int] -> Map [a]
-- selectElems ps xs = map (xs !!) ps

-- | Cut off indexed elements.
--
--   >>> selectIndex "ce" "abcdefg" `selectOthers` "ABCDEFG"
--   "ABDFG"
--
selectOthers :: Select a
selectOthers ps xs = loop 0 xs where
    loop _ [] = []
    loop p (x:xs2)
        | p `elem` ps  =     loop (p + 1) xs2
        | otherwise    = x : loop (p + 1) xs2

-- | Pair of picking-up and cutting-off elements.
--
--   >>> selectIndex "ce" "abcdefg" `selectBoth` "ABCDEFG"
--   ("CE", "ABDFG")
--
selectBoth :: [Int] -> [a] -> ([a], [a])
selectBoth ps xs = (selectElems ps xs, selectOthers ps xs)


-- --------------------------------------------  Trisection

-- | Take shared elements.
--
--   >>> "abcd" `selectShare` "cbefg"
--   "bc"
--
selectShare :: (Ord a) => O.Bin [a]
selectShare = flip selectShareRight

selectShareRight :: (Ord a) => O.Bin [a]
selectShareRight xs = filter (`Set.member` Set.fromList xs)

-- | Take left-side elements.
--
--   >>> "abcd" `selectLeft` "bcefg"
--   "ad"
--
selectLeft :: (Ord a) => O.Bin [a]
selectLeft = flip selectRight

-- | Take right-side elements.
--
--   >>> "abcd" `selectRight` "bcefg"
--   "efg"
--
selectRight :: (Ord a) => O.Bin [a]
selectRight xs = filter (`Set.notMember` Set.fromList xs)


-- --------------------------------------------  Permutation

-- | Move indexed elements to the front.
--
--   >>> selectIndex "cd" "abcdefg" `permuteForward` "ABCDEFG"
--   "CDABEFG"
--
permuteForward :: Select a
permuteForward ps xs = case selectBoth ps xs of
                         (select, others) -> select ++ others

-- | Move indexed elements to the rear.
--
--   >>> selectIndex "cd" "abcdefg" `permuteBackward` "ABCDEFG"
--   "ABEFGCD"
--
permuteBackward :: Select a
permuteBackward ps xs = case selectBoth ps xs of
                          (select, others) -> others ++ select

-- | Double forward.
permuteForward2 :: Select2 a b
permuteForward2 = (permuteForward, permuteForward)

-- | Double backward.
permuteBackward2 :: Select2 a b
permuteBackward2 = (permuteBackward, permuteBackward)

-- | Reorder elements.
--
--   >>> permuteOrder "bca" "abc" "ABC"
--   "BCA"
--
permuteOrder :: (Eq a) => [a] -> [a] -> O.Map [c]
permuteOrder to from
    | to == from = id
    | otherwise  = selectElems $ selectIndex to from
