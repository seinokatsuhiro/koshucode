{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.List.List
  ( -- * List
    tails,
    isSingleton,
    notNull,
    maybeEmpty,
    right,

    -- * Take
    takeFirst, takeLast,
    takeOdd, takeEven,
    takeFill, takeTailFill,
  
    -- * Uniqueness
    duplicates, unique,
    unionUp, intersectionFilter,
    setList, setEq,
    -- $Uniqueness
  
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

import qualified Data.List                      as List
import qualified Data.Set                       as Set
import qualified Koshucode.Baala.Base.Prelude   as B


-- ----------------------  List

-- | Tails of list.
--
--   >>> tails "abc"
--   ["abc", "bc", "c"]
tails :: [a] -> [[a]]
tails = loop where
    loop [] = []
    loop xxs@(_:xs) = xxs : loop xs

-- | Test list has single element.
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton  _  = False

-- | Test list is not empty.
notNull :: [a] -> Bool
notNull = not . null

maybeEmpty :: Maybe a -> (a -> [b]) -> [b]
maybeEmpty m f = maybe [] f m

right :: b -> B.Map (Either a b)
right _ (Right x) = Right x
right x (Left _)  = Right x


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


-- ----------------------  Uniqueness

-- $Uniqueness
--
--   /Examples/
--
--   >>> duplicates "banana"
--   "ana"
--
--   >>> unique "banana"
--   "ban"
--
--   >>> unionUp "cde" "abc"
--   "deabc"
--
--   >>> List.union "cde" "abc"
--   "cdeab"
--

-- | Keep duplicate elements.
duplicates :: (Ord a) => [a] -> [a]
duplicates xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = x : loop xs2 set
        | otherwise        = loop xs2 (Set.insert x set)

-- | Remove duplicate elements.
unique :: (Ord a) => [a] -> [a]
unique xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = loop xs2 set
        | otherwise        = x : loop xs2 (Set.insert x set)

-- | Union list to base list.
unionUp :: (Eq a) => [a] -> [a] -> [a]
unionUp xs ys = (xs List.\\ ys) ++ ys

-- intersectionFilter "abcd" "dxcy"
-- >>> "dc"
intersectionFilter :: (Ord a) => [a] -> [a] -> [a]
intersectionFilter xs = filter (`Set.member` Set.fromList xs)

-- | Convert to set-like list, in other words,
--   remove duplicate elements and sort list.
setList :: (Ord a) => [a] -> [a]
setList = Set.toAscList . Set.fromList

-- | Set-like equality, in other words, 
--   duplication and ordering are ignored.
setEq :: (Ord a) => [a] -> [a] -> Bool
setEq xs ys = (Set.fromList xs) == (Set.fromList ys)


-- ----------------------  Construct

-- | Make singleton list.
li1 :: a -> [a]
li1 x = [x]

li2 :: a -> a -> [a]
li2 x y = [x, y]

li3 :: a -> a -> a -> [a]
li3 x y z = [x, y, z]

consIf :: Bool -> a -> [a] -> [a]
consIf True  x xs  = x : xs
consIf False _ xs  = xs

type RangeBy a = a -> a -> [a]

rangeBy :: (Ord a) => B.Map a -> RangeBy a
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

mapAt :: (B.Map a) -> Int -> B.Map [a]
mapAt f = loop where
    loop 0 (x : xs) = f x : xs
    loop i (x : xs) = x : loop (i - 1) xs
    loop _ []       = []

mapWithLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapWithLast f g = loop where
    loop [] = []
    loop [x] = [g x]
    loop (x:xs) = f x : loop xs

omit :: (a -> Bool) -> B.Map [a]
omit f = filter $ not . f

filterFst :: (a -> Bool) -> B.Map [(a, b)]
filterFst p = filter (p . fst)

squeeze :: (a -> Bool) -> B.Map [a]
squeeze p = loop where
    loop (x1 : x2 : xs)
        | p x1 && p x2 = x2 : squeeze p xs
        | otherwise    = x1 : squeeze p (x2 : xs)
    loop xs = xs

squeezeEmptyLines :: B.Map [String]
squeezeEmptyLines = squeeze $ null . dropWhile (== ' ')

reverseMap :: B.Map [a] -> B.Map [a]
reverseMap f = reverse . f . reverse
