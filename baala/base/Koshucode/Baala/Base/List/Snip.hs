{-# OPTIONS_GHC -Wall #-}

-- | Snipping elements.

module Koshucode.Baala.Base.List.Snip
  ( -- * Type
    Snip, Snip2, Bin,
  
    -- * Function
    snipFull, snipIndex, snipPair,
    snipBoth, snipFrom, snipOff,
    -- $FunctionExample
  
    -- * Derivative
    snipForward, snipBackward, 
    snipForward2, snipBackward2,
    snipLeft, snipShare, snipRight,
    snipOrder,
    sameLength, notSameLength,
    (+-),
    -- $DerivativeExample
  
  ) where

import qualified Data.List                           as List
import qualified Koshucode.Baala.Base.Prelude        as B


-- --------------------------------------------  Type

-- | Snipping elements using indicies.
type Snip a = [Int] -> B.Map [a]

-- | Snip for differenct type values.
type Snip2 a b = (Snip a, Snip b)

-- | Type for binary operators.
type Bin a = a -> a -> a


-- --------------------------------------------  Function

-- $FunctionExample
--
--  /Examples/
--
--  Obtain indicies for snipping @\"bdk\"@ from @\"abcdefg\"@.
--  The element @\"k\"@ is ignored because it is not in  @\"abcdefg\"@.
--
--    >>> snipIndex "bdk" "abcdefg"
--    [1,3]
--
--  Pick up same-position elements.
--
--    >>> [1,3] `snipFrom` "abcdefg"
--    "bd"
--    >>> [1,3] `snipFrom` "ABCDEFG"
--    "BD"
--
--  Cut off same-position elements.
--
--    >>> (snipIndex "ce" "abcdefg") `snipOff` "ABCDEFG"
--    "ABDFG"
--
--  Get pick-up and cut-off elements.
--
--    >>> (snipIndex "ce" "abcdefg") `snipBoth` "ABCDEFG"
--    ("CE", "ABDFG")

snipFull :: (Eq a) => [a] -> [a] -> [Int]
snipFull ks xs = map ind ks where
    ind k = maybe (-1) id $ List.elemIndex k xs

-- | Indices of shared elements.
snipIndex :: (Eq a) => [a] -> [a] -> [Int]
snipIndex ks xs = filter (>= 0) $ snipFull ks xs

snipPair :: (Ord a) => [a] -> [a] -> ([Int], [Int])
snipPair xs1 xs2 = (snipIndex sh xs1, snipIndex sh xs2) where
    ind = snipIndex xs1 xs2
    sh  = B.sort $ snipFrom ind xs2

-- | Pair of picking-up and cutting-off elements.
snipBoth :: [Int] -> [a] -> ([a], [a])
snipBoth ps xs = (snipFrom ps xs, snipOff ps xs)

-- | Pick up indexed elements.
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
snipOff :: Snip a
snipOff ps xs = loop 0 xs where
    loop _ [] = []
    loop p (x:xs2)
        | p `elem` ps  =     loop (p + 1) xs2
        | otherwise    = x : loop (p + 1) xs2


-- --------------------------------------------  Derivative

-- $DerivativeExample
--
-- /Examples/
--
--  Move snipping elements to the front and rear.
--
--    >>> (snipIndex "cd" "abcdefg") `snipForward` "ABCDEFG"
--    "CDABEFG"
--    >>> (snipIndex "cd" "abcdefg") `snipBackward` "ABCDEFG"
--    "ABEFGCD"
--
--  Shared elements of @\"abcd\"@ and @\"bcefg\"@.
--
--    >>> "abcd" `snipShare` "bcefg"
--    "bc"
--
--  Left-side and right-side.
--
--    >>> "abcd" `snipLeft` "bcefg"
--    "ad"
--    >>> "abcd" `snipRight` "bcefg"
--    "efg"
--
--  Check elements in the first list are non-negative,
--  and second are negative.
--
--    >>> [0, 1] +- [-1]
--    True
--    >>> [0] +- [1, -1]
--    False

-- | Move indexed elements to the front.
snipForward :: Snip a
snipForward ps xs = case snipBoth ps xs of
                      (snip, rest) -> snip ++ rest

-- | Move indexed elements to the rear.
snipBackward :: Snip a
snipBackward ps xs = case snipBoth ps xs of
                       (snip, rest) -> rest ++ snip

snipForward2 :: Snip2 a b
snipForward2 = (snipForward, snipForward)

snipBackward2 :: Snip2 a b
snipBackward2 = (snipBackward, snipBackward)

-- | Take left-side elements.
snipLeft :: (Eq a) => Bin [a]
snipLeft xs ys = snipIndex ys xs `snipOff` xs

-- | Take shared elements.
snipShare :: (Eq a) => Bin [a]
snipShare xs ys = snipIndex xs ys `snipFrom` ys

-- | Take right-side elements.
snipRight :: (Eq a) => Bin [a]
snipRight xs ys = snipLeft ys xs

snipOrder :: (Eq a) => [a] -> [a] -> B.Map [c]
snipOrder to from
    | to == from = id
    | otherwise  = snipFrom $ snipIndex to from

-- | Check lengths of two lists are same.
sameLength :: [a] -> [b] -> Bool
sameLength a b = length a == length b

notSameLength :: [a] -> [b] -> Bool
notSameLength a b = not $ sameLength a b

-- | Check elements in the first list are non-negative,
--   and elements in the second are negative.
(+-) :: [Int] -> [Int] -> Bool
(+-) pos neg = all (>= 0) pos
            && all (< 0)  neg
