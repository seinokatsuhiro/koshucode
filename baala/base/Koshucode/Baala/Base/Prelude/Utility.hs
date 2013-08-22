{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC #-}

{-| General utilities -}

module Koshucode.Baala.Base.Prelude.Utility
(
  -- * Pair
  mapFst,
  mapSnd,
  cons1,
  mapmapFst,
  mapmapSnd,
  maybePairs,

  -- * List
  unique,
  unionUp,
  singleton,
  splitBy,
  divide,
  divideBy,

  -- * String
  padRight,
  padLeft,

  -- * Gather
  gather,
  gatherWith,
  gatherToMap,

  -- * Lookup
  lookupSatisfy,
  lookupMap,

) where

import Control.Applicative
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import Koshucode.Baala.Base.Prelude.Class



-- ----------------------  Pair

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (x,y) = (f x,y)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (x,y) = (x,f y)

cons1 :: a -> ([a], b) -> ([a], b)
cons1 x xs = mapFst (x:) xs

mapmapFst :: (a -> c) -> [(a,b)] -> [(c,b)]
mapmapFst = map . mapFst

mapmapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapmapSnd = map . mapSnd

maybePairs :: [a] -> Maybe [(a,a)]
maybePairs (a:b:xs) = liftA ((a,b):) $ maybePairs xs
maybePairs []       = Just []
maybePairs _        = Nothing



-- ----------------------  List

{-| Remove duplicate elements. -}
unique :: (Ord a) => [a] -> [a]
unique xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = loop xs2 set
        | otherwise        = x : loop xs2 (Set.insert x set)

{-| Union list to base list.

    >>> unionUp "cde" "abc"
    "deabc"

    >>> List.union "cde" "abc"
    "cdeab"  -}
unionUp
    :: (Eq a)
    => [a]  -- ^ Append list
    -> [a]  -- ^ Base list
    -> [a]  -- ^ Result
unionUp xs ys = (xs List.\\ ys) ++ ys

{-| Make singleton list. -}
singleton :: a -> [a]
singleton x = [x]

{-| Split list by predicate.
    If list contains an element that satisfies the predicate,
    @(/before-list/, /the-element/, /after-list/)@ is returned.
    Otherwise, original list is returned.

    >>> splitBy (== '|') "b c"
    Left "b c"

    >>> splitBy (== '|') "a | b | c"
    Right ("a ", '|', " b | c")  -}
splitBy
    :: (a -> Bool)
    -> [a]
    -> Either [a] ([a], a, [a])
splitBy p xs =
    case break p xs of
      (a, x : b) -> Right (a, x, b)
      _          -> Left xs

{-| Divide list.

    >>> divide '|' "a|bb||ccc|"
    ["a", "bb", "", "ccc", ""]  -}
divide :: (Eq a) => a -> [a] -> [[a]]
divide dv = divideBy (== dv)

{-| Divide list.

    >>> divideBy (== '|') "a|bb||ccc|"
    ["a", "bb", "", "ccc", ""]  -}
divideBy :: (a -> Bool) -> [a] -> [[a]]
divideBy p = loop where
    loop xs = case break p xs of
                (x, _ : xs2) -> x : loop xs2
                (x, [])      -> [x]

stringWidth :: String -> Int
stringWidth = sum . map charWidth

charWidth :: Char -> Int
charWidth c
    | Char.ord c >= 256 = 2
    | otherwise         = 1



-- ----------------------  String

{-| Add spaces to right.

    >>> padRight 10 "abc"
    "abc       "  -}
padRight :: Int -> Map String
padRight n s = s ++ replicate rest ' ' where
    rest = max 0 (n - stringWidth s)

{-| Add spaces to left.

    >>> padLeft 10 "abc"
    "       abc"  -}
padLeft :: Int -> Map String
padLeft n s = replicate rest ' ' ++ s where
    rest = max 0 (n - stringWidth s)



-- ----------------------  Gather

{-| Gather what is gotten by splitter. -}
gather :: ([a] -> (b, [a])) -> [a] -> [b]
gather one = loop where
    loop [] = []
    loop xs = let (y, xs2) = one xs
              in y : loop xs2

gatherWith :: (c -> [a] -> (b, [a])) -> [c] -> [a] -> [b]
gatherWith f = loop where
    loop [] _ = []
    loop _ [] = []
    loop (c:cs) as = let (b, as') = f c as
                     in b : loop cs as'

{-| Gather (/key/, /value/) to 'Map.Map' /key/ [/value/]. -}
gatherToMap :: (Ord k) => [(k,v)] -> Map.Map k [v]
gatherToMap xs = loop xs Map.empty where
    loop [] m = m
    loop ((k,v) : xs2) m =
        case Map.lookup k m of
          Just vs -> loop xs2 $ Map.insert k (v:vs) m
          Nothing -> loop xs2 $ Map.insert k [v] m



-- ----------------------  Lookup

lookupSatisfy :: a -> [(a -> Bool, b)] -> Maybe b
lookupSatisfy x = loop where
    loop [] = Nothing
    loop ((p, v) : ps)
        | p x = Just v
        | otherwise = loop ps

lookupMap :: (Ord k) => k -> Map.Map k a -> Maybe a
lookupMap = Map.lookup

