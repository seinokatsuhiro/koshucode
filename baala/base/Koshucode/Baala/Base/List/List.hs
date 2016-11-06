{-# OPTIONS_GHC -Wall #-}

-- | List utilities.

module Koshucode.Baala.Base.List.List
  ( -- * List
    tails,
    maybeEmpty,
    right,
    isSingleton,
    notNull,
    sameLength, notSameLength,

    -- * Take
    takeFirst, takeLast,
    takeOdd, takeEven,
    takeFill, takeTailFill,
  
    -- * Construct
    li1, li2, li3,
    consIf,
    RangeBy, rangeBy,
    zipMaybe, zipMaybe2,

    -- * Map
    map2, mapAt, mapWithLast,
    omit, filterFst, squeeze, squeezeEmptyLines,
    reverseMap,
  ) where

import qualified Koshucode.Baala.Overture       as O


-- ----------------------  List

-- | Tails of list.
--
--   >>> tails "abc"
--   ["abc", "bc", "c"]
tails :: [a] -> [[a]]
tails = loop where
    loop [] = []
    loop xxs@(_:xs) = xxs : loop xs

maybeEmpty :: Maybe a -> (a -> [b]) -> [b]
maybeEmpty m f = maybe [] f m

-- | Replace 'Left' value to 'Right' value.
right :: b -> O.Map (Either a b)
right _ (Right x) = Right x
right x (Left _)  = Right x

-- | Test list has single element.
isSingleton :: O.Test [a]
isSingleton [_] = True
isSingleton  _  = False

-- | Test list is not empty.
notNull :: O.Test [a]
notNull = not . null

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
takeLast :: [a] -> [a]
takeLast [] = []
takeLast xs = [last xs]

-- | Take elements at odd positions, i.e., first, third, ...
--
--   >>> takeOdd "abcdeft"
--   "acet"

takeOdd :: [a] -> [a]
takeOdd []  = []
takeOdd [x] = [x]
takeOdd (x : _ : xs) = x : takeOdd xs

-- | Take elements at even positions. i.e., second, fourth, ...
--
--   >>> takeEven "abcdeft"
--   "bdf"

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

takeTailFill :: a -> Int -> [a] -> [a]
takeTailFill fill n = reverseMap $ takeFill fill n


-- ----------------------  Construct

-- | Make singleton list.
--
--   >>> li1 'a'
--   "a"
--
li1 :: a -> [a]
li1 x = [x]

-- | Make two-element list.
--
--   >>> li2 'a' 'b'
--   "ab"
--
li2 :: a -> a -> [a]
li2 x y = [x, y]

-- | Make three-element list.
--
--   >>> li3 'a' 'b' 'c'
--   "abc"
--
li3 :: a -> a -> a -> [a]
li3 x y z = [x, y, z]

consIf :: Bool -> a -> [a] -> [a]
consIf True  x xs  = x : xs
consIf False _ xs  = xs

type RangeBy a = a -> a -> [a]

rangeBy :: (Ord a) => O.Map a -> RangeBy a
rangeBy step from to = loop from where
    loop f | f > to    = []
           | otherwise = f : loop (step f)

zipMaybe :: [Maybe a] -> [b] -> [(a, b)]
zipMaybe (Just a  : as) (b : bs)  = (a, b) : zipMaybe as bs
zipMaybe (Nothing : as) (_ : bs)  =          zipMaybe as bs
zipMaybe _  _                     = []

zipMaybe2 :: [Maybe a] -> [b] -> [b]
zipMaybe2 (Just _  : as) (b : bs)  = b : zipMaybe2 as bs
zipMaybe2 (Nothing : as) (_ : bs)  =     zipMaybe2 as bs
zipMaybe2 _  _                     = []


-- ----------------------  Map

map2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
map2 = fmap . fmap

-- map2 :: (a -> b) -> [[a]] -> [[b]]
-- map2 = map . map

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

mapWithLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapWithLast f g = loop where
    loop [] = []
    loop [x] = [g x]
    loop (x:xs) = f x : loop xs

-- | Omit elements, i.e., anti-'filter'.
--
--   >>> omit (`elem` "ae") "abcdefg"
--   "bcdfg"
--
omit :: (a -> Bool) -> O.Map [a]
omit f = filter $ not . f

filterFst :: (a -> Bool) -> O.Map [(a, b)]
filterFst p = filter (p . fst)

squeeze :: (a -> Bool) -> O.Map [a]
squeeze p = loop where
    loop (x1 : x2 : xs)
        | p x1 && p x2 = x2 : squeeze p xs
        | otherwise    = x1 : squeeze p (x2 : xs)
    loop xs = xs

squeezeEmptyLines :: O.Map [String]
squeezeEmptyLines = squeeze $ null . dropWhile (== ' ')

reverseMap :: O.Map [a] -> O.Map [a]
reverseMap f = reverse . f . reverse

