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
  divideBy,
  divideByP,
  padRight,
  padLeft,

  -- * Collection
  gather,
  gatherWith,
  gatherToMap,
  lookupSatisfy,
  lookupMap,

) where

import Control.Applicative
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import Koshucode.Baala.Base.Prelude.Class



-- ----------------------

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



-- ----------------------

{-| Remove duplicate elements. -}
unique :: (Ord a) => [a] -> [a]
unique xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = loop xs2 set
        | otherwise        = x : loop xs2 (Set.insert x set)

unionUp :: (Eq a) => [a] -> [a] -> [a]
unionUp xs ys = (xs List.\\ ys) ++ ys

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

{-| Gather (key,value) to a Map key [value] -}
gatherToMap :: (Ord k) => [(k,v)] -> Map.Map k [v]
gatherToMap xs = loop xs Map.empty where
    loop [] m = m
    loop ((k,v) : xs2) m =
        case Map.lookup k m of
          Just vs -> loop xs2 $ Map.insert k (v:vs) m
          Nothing -> loop xs2 $ Map.insert k [v] m

lookupSatisfy :: a -> [(a -> Bool, b)] -> Maybe b
lookupSatisfy x = loop where
    loop [] = Nothing
    loop ((p, v) : ps)
        | p x = Just v
        | otherwise = loop ps

lookupMap :: (Ord k) => k -> Map.Map k a -> Maybe a
lookupMap = Map.lookup

{-| Singleton list -}
singleton :: a -> [a]
singleton x = [x]

{-| Divide list.

    >>> divideBy '|' "a|bb||ccc|"
    ["a", "bb", "", "ccc", ""]  -}
divideBy :: (Eq a) => a -> [a] -> [[a]]
divideBy dv = divideByP (== dv)

{-| Divide list.

    >>> divideByP (== '|') "a|bb||ccc|"
    ["a", "bb", "", "ccc", ""]  -}
divideByP :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
divideByP p = loop where
    loop xs = case break p xs of
                (x, _ : xs2) -> x : loop xs2
                (x, [])      -> [x]

stringWidth :: String -> Int
stringWidth = sum . map charWidth

charWidth :: Char -> Int
charWidth c
    | Char.ord c >= 256 = 2
    | otherwise         = 1

{-| Add spaces.

    >>> padRight 10 "abc"
    "abc       "
 -}
padRight :: Int -> Map String
padRight n s = s ++ replicate rest ' ' where
    rest = max 0 (n - stringWidth s)

{-| Add spaces.

    >>> padLeft 10 "abc"
    "       abc"
 -}
padLeft :: Int -> Map String
padLeft n s = replicate rest ' ' ++ s where
    rest = max 0 (n - stringWidth s)

