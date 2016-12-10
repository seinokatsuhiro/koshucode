{-# OPTIONS_GHC -Wall #-}

-- | Snipping elements.

module Koshucode.Baala.Base.List.Snip
  ( -- * Type
    Snip, Snip2,
  
    -- * Index
    snipIndex, snipIndexFull, snipIndexBoth,
  
    -- * Picking elements
    snipFrom, snipOff, snipBoth,
    snipShare, snipLeft, snipRight,

    -- * Reorder elements
    snipForward, snipBackward, 
    snipForward2, snipBackward2,
    snipOrder,
  ) where

import qualified Data.List                           as List
import qualified Data.Set                            as Set
import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base.Prelude        as B


-- --------------------------------------------  Type

-- | Snipping elements using indicies.
type Snip a = [Int] -> O.Map [a]

-- | Snip for differenct type values.
type Snip2 a b = (Snip a, Snip b)


-- --------------------------------------------  Index

-- | Indices of shared elements.
--
--   >>> snipIndex "bdk" "abcdefg"
--   [1,3]
--
snipIndex :: (Eq a) => [a] -> [a] -> [Int]
snipIndex ks xs = filter (>= 0) $ snipIndexFull ks xs

-- | Indicies of shared-and-unknown elements.
--
--   >>> snipIndexFull "bcx" "abcdefg"
--   [1,2,-1]
--
snipIndexFull :: (Eq a) => [a] -> [a] -> [Int]
snipIndexFull ks xs = map ind ks where
    ind k = maybe (-1) id $ List.elemIndex k xs

-- | Left-and-right indices of shared elements.
--
--   >>> snipIndexBoth "abc" "bcd"
--   ([1,2], [0,1])
--
snipIndexBoth :: (Ord a) => [a] -> [a] -> ([Int], [Int])
snipIndexBoth xs1 xs2 = (snipIndex sh xs1, snipIndex sh xs2) where
    ind = snipIndex xs1 xs2
    sh  = B.sort $ snipFrom ind xs2


-- --------------------------------------------  Picking elements

-- | Pick up indexed elements.
--
--   >>> [1,3] `snipFrom` "abcdefg"
--   "bd"
--
--   >>> [1,3] `snipFrom` "ABCDEFG"
--   "BD"
--
snipFrom :: Snip a
snipFrom ps xs = loop ps xs 0 where
    loop pps@(p:ps2) (x:xs2) i =
        case compare p i of
          EQ             -> x : loop ps2 xs2 (i + 1)
          GT             ->     loop pps xs2 (i + 1)
          LT | p >= 0    -> snipFrom pps xs  -- restart
             | otherwise -> snipFrom ps2 xs  -- ignore minus index
    loop pps@(p:ps2) [] i
        | p < i     = snipFrom pps xs  -- restart
        | otherwise = snipFrom ps2 xs  -- ignore large index
    loop [] _ _ = []

-- Simple implementation
-- snipFrom :: [Int] -> Map [a]
-- snipFrom ps xs = map (xs !!) ps

-- | Cut off indexed elements.
--
--   >>> (snipIndex "ce" "abcdefg") `snipOff` "ABCDEFG"
--   "ABDFG"
--
snipOff :: Snip a
snipOff ps xs = loop 0 xs where
    loop _ [] = []
    loop p (x:xs2)
        | p `elem` ps  =     loop (p + 1) xs2
        | otherwise    = x : loop (p + 1) xs2

-- | Pair of picking-up and cutting-off elements.
--
--   >>> (snipIndex "ce" "abcdefg") `snipBoth` "ABCDEFG"
--   ("CE", "ABDFG")
--
snipBoth :: [Int] -> [a] -> ([a], [a])
snipBoth ps xs = (snipFrom ps xs, snipOff ps xs)

-- | Take shared elements.
--
--   >>> "abcd" `snipShare` "cbefg"
--   "bc"
--
snipShare :: (Ord a) => O.Bin [a]
snipShare = flip snipShareRight

snipShareRight :: (Ord a) => O.Bin [a]
snipShareRight xs = filter (`Set.member` Set.fromList xs)

-- | Take left-side elements.
--
--   >>> "abcd" `snipLeft` "bcefg"
--   "ad"
--
snipLeft :: (Ord a) => O.Bin [a]
snipLeft = flip snipRight

-- | Take right-side elements.
--
--   >>> "abcd" `snipRight` "bcefg"
--   "efg"
--
snipRight :: (Ord a) => O.Bin [a]
snipRight xs = filter (`Set.notMember` Set.fromList xs)


-- --------------------------------------------  Reorder elements

-- | Move indexed elements to the front.
--
--   >>> (snipIndex "cd" "abcdefg") `snipForward` "ABCDEFG"
--   "CDABEFG"
--
snipForward :: Snip a
snipForward ps xs = case snipBoth ps xs of
                      (snip, rest) -> snip ++ rest

-- | Move indexed elements to the rear.
--
--   >>> (snipIndex "cd" "abcdefg") `snipBackward` "ABCDEFG"
--   "ABEFGCD"
--
snipBackward :: Snip a
snipBackward ps xs = case snipBoth ps xs of
                       (snip, rest) -> rest ++ snip

-- | Double forward.
snipForward2 :: Snip2 a b
snipForward2 = (snipForward, snipForward)

-- | Double backward.
snipBackward2 :: Snip2 a b
snipBackward2 = (snipBackward, snipBackward)

-- | Reorder elements.
--
--   >>> snipOrder "bca" "abc" "ABC"
--   "BCA"
--
snipOrder :: (Eq a) => [a] -> [a] -> O.Map [c]
snipOrder to from
    | to == from = id
    | otherwise  = snipFrom $ snipIndex to from
