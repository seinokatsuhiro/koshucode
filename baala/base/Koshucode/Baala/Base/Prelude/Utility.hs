{-# OPTIONS_GHC -Wall #-}

-- | General utilities

module Koshucode.Baala.Base.Prelude.Utility
(
-- * Pair
  mapFst, mapSnd, cons1
, mapmapFst, mapmapSnd
, maybePairs
, Named
-- * List
, unique, unionUp
, singleton, divideBy
-- * Collection
, gather, gatherToMap, lookupMap
-- * Application
, (|>), ($>)
-- * Class
, Name (..)
) where

import Control.Applicative
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

type Named a = (String, a)

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

-- | Composition of functions
(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = g . f

-- | Function application
($>) :: a -> (a -> b) -> b
x $> f = f x

infixr 6 |>
infix  5 $>

-- | Remove duplicate elements
unique :: (Ord a) => [a] -> [a]
unique xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = loop xs2 set
        | otherwise        = x : loop xs2 (Set.insert x set)

unionUp :: (Eq a) => [a] -> [a] -> [a]
unionUp xs ys = (xs List.\\ ys) ++ ys

gather :: ([a] -> (b, [a])) -> [a] -> [b]
gather f = loop where
    loop [] = []
    loop xs = let (next, xs2) = f xs
              in next : gather f xs2

-- gather (key,value) to a Map key [value]
gatherToMap :: (Ord k) => [(k,v)] -> Map.Map k [v]
gatherToMap xs = loop xs Map.empty where
    loop [] m = m
    loop ((k,v) : xs2) m =
        case Map.lookup k m of
          Just vs -> loop xs2 $ Map.insert k (v:vs) m
          Nothing -> loop xs2 $ Map.insert k [v] m

lookupMap :: (Ord k) => k -> Map.Map k a -> Maybe a
lookupMap = Map.lookup

-- | Singleton list
singleton :: a -> [a]
singleton x = [x]

divideBy :: (Eq a) => a -> [a] -> [[a]]
divideBy dv = loop where
    loop xs = case break (== dv) xs of
                (x, _ : xs2) -> x : loop xs2
                (x, [])      -> [x]

-- | Types that has name
class Name a where
    name :: a -> String
    names :: [a] -> [String]
    names = map name

