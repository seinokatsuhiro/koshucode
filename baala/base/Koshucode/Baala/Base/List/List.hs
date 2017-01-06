{-# OPTIONS_GHC -Wall #-}

-- | List utilities.

module Koshucode.Baala.Base.List.List
  ( -- * Test
    notNull, isSingleton,
    sameLength, notSameLength,

    -- * Take
    takeFirst, takeLast,
    takeMiddle, takeEnd,
    takeOdd, takeEven,
    takeFill, takeTailFill,
    takeChunks,
  
    -- * Construct
    list1, list2, list3,
    consIf,
    RangeBy, rangeBy,
    zipMaybe, zipMaybe2,

    -- * Map
    mapAt,
    omit,
    squeeze, squeezeEmptyLines,
  ) where

import qualified Koshucode.Baala.Overture         as O
import qualified Koshucode.Baala.Base.List.Split  as B


-- ----------------------  List

-- | Test list is not empty.
notNull :: O.Test [a]
notNull = not . null

-- | Test list has single element.
isSingleton :: O.Test [a]
isSingleton [_] = True
isSingleton  _  = False

-- | Test lengths of two lists are same.
--
--   >>> sameLength "abc" "ABC"
--   True
--
sameLength :: O.Test2 [a] [b]
sameLength a b = length a == length b

-- | Test lengths of two lists are not same.
--
--   >>> notSameLength "abc" "ABC"
--   False
--
notSameLength :: O.Test2 [a] [b]
notSameLength a b = not $ sameLength a b


-- ----------------------  Take

-- | Take first element; if empty list is given, returns empty list.
--
--   >>> takeFirst "abc"
--   "a"
--
--   >>> takeFirst ""
--   ""
takeFirst :: [a] -> [a]
takeFirst = take 1

-- | Take last element; if empty list is given, returns empty list.
--
--   >>> takeLast "abc"
--   "c"
--
takeLast :: [a] -> [a]
takeLast [] = []
takeLast xs = [last xs]

-- | Take middle /N/ elements.
--
--   >>> (`takeMiddle` ['A' .. 'Z']) <$> [-1 .. 7 :: Int]
--   ["", "", "M", "MN", "LMN", "LMNO", "KLMNO", "KLMNOP", "JKLMNOP"]
--
takeMiddle :: Int -> [a] -> [a]
takeMiddle n xs
    | l <= n    = xs
    | otherwise = take n $ drop h xs
    where l = length xs
          h = (l - n) `div` 2

-- | Take /N/ elements at the end of the list.
--
--   >>> takeEnd 3 "ABCDEFG"
--   "EFG"
--
takeEnd :: Int -> [a] -> [a]
takeEnd n = (take n O./$/)

-- | Take elements at odd positions, i.e., first, third, ...
--
--   >>> takeOdd "abcdeft"
--   "acet"
--
takeOdd :: [a] -> [a]
takeOdd []  = []
takeOdd [x] = [x]
takeOdd (x : _ : xs) = x : takeOdd xs

-- | Take elements at even positions. i.e., second, fourth, ...
--
--   >>> takeEven "abcdeft"
--   "bdf"
--
takeEven :: [a] -> [a]
takeEven []  = []
takeEven [_] = []
takeEven (_ : x : xs) = x : takeEven xs

-- | Take /N/ elements with filler.
--
--   >>> takeFill 0 3 [0 .. 8]
--  [0,1,2]
--
--   >>> takeFill 0 5 [6 .. 8]
--  [6,7,8,0,0]
--
takeFill :: a -> Int -> [a] -> [a]
takeFill fill = loop where
    loop 0 _       = []
    loop n []      = fill : loop (n - 1) []
    loop n (x:xs)  =    x : loop (n - 1) xs

-- | Take tail-side /N/ elements with filler.
--
--   >>> takeTailFill 0 3 [0 .. 8]
--  [6,7,8]
--
--   >>> takeTailFill 0 5 [6 .. 8]
--  [0,0,6,7,8]
--
takeTailFill :: a -> Int -> [a] -> [a]
takeTailFill fill n = (takeFill fill n O./$/)

-- | Take indexed chunks from /N/ chunks.
--
--   >>> takeChunks 20 [10, 0, 19, 20] [0 .. 98 :: Int]
--   [[50,51,52,53,54], [0,1,2,3,4], [95,96,97,98], []]
--
takeChunks :: Int -> [Int] -> [a] -> [[a]]
takeChunks per ps xs = f <$> ps where
    f p | p < 0     = []
        | p >= per  = []
        | otherwise = B.divideInto per xs !! p


-- ----------------------  Construct

-- | Make singleton list.
--
--   >>> list1 'a'
--   "a"
--
{-# INLINE list1 #-}
list1 :: a -> [a]
list1 x = [x]

-- | Make two-element list.
--
--   >>> list2 'a' 'b'
--   "ab"
--
{-# INLINE list2 #-}
list2 :: a -> a -> [a]
list2 x y = [x, y]

-- | Make three-element list.
--
--   >>> list3 'a' 'b' 'c'
--   "abc"
--
{-# INLINE list3 #-}
list3 :: a -> a -> a -> [a]
list3 x y z = [x, y, z]

-- | Conditional cons up.
consIf :: Bool -> a -> [a] -> [a]
consIf True  x xs  = x : xs
consIf False _ xs  = xs

-- | Range generation
type RangeBy a = a -> a -> [a]

-- | Generate range of something.
--
--   >>> rangeBy (+ 3) 0 10
--   [0,3,6,9]
--
rangeBy :: (Ord a) => O.Map a -> RangeBy a
rangeBy step from to = loop from where
    loop f | f > to    = []
           | otherwise = f : loop (step f)

-- | Zip optoinal elements.
--
--   >>> zipMaybe [Just 'a', Nothing] "AB"
--   [('a','A')]
--
zipMaybe :: [Maybe a] -> [b] -> [(a, b)]
zipMaybe (Just a  : as) (b : bs)  = (a, b) : zipMaybe as bs
zipMaybe (Nothing : as) (_ : bs)  =          zipMaybe as bs
zipMaybe _  _                     = []

-- | Zip filter.
--
--   >>> zipMaybe2 [Just 'a', Nothing] "AB"
--   [('a','A')]
--
zipMaybe2 :: [Maybe a] -> [b] -> [b]
zipMaybe2 (Just _  : as) (b : bs)  = b : zipMaybe2 as bs
zipMaybe2 (Nothing : as) (_ : bs)  =     zipMaybe2 as bs
zipMaybe2 _  _                     = []


-- ----------------------  Map

-- | Apply function to specific element.
--
--   >>> mapAt (const '-') 3 "abcdefg"
--   "abc-efg"
--
mapAt :: O.Map a -> Int -> O.Map [a]
mapAt f = loop where
    loop 0 (x : xs) = f x : xs
    loop i (x : xs) = x : loop (i - 1) xs
    loop _ []       = []

-- | Omit elements, i.e., anti-'filter'.
--
--   >>> omit (`elem` "ae") "abcdefg"
--   "bcdfg"
--
omit :: (a -> Bool) -> O.Map [a]
omit f = filter $ not . f

-- | Compress continued multile elements into single elements.
--
--   >>> squeeze (== '?') "Koshu???"
--   "Koshu?"
--
squeeze :: (a -> Bool) -> O.Map [a]
squeeze p = loop where
    loop (x1 : xxs@(x2 : _))
        | p x1 && p x2 = squeeze p xxs
        | otherwise    = x1 : squeeze p xxs
    loop xs = xs

-- | Compress continued white lines.
--
--   >>> squeezeEmptyLines ["a", "b", " ", "c", "", " ", "  ", "d"]
--   ["a", "b", " ", "c", "  ", "d"]
--
squeezeEmptyLines :: O.Map [String]
squeezeEmptyLines = squeeze $ null . dropWhile (== ' ')

