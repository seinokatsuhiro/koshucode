{-# OPTIONS_GHC -Wall #-}

-- | Utilities for association lists.

module Koshucode.Baala.Base.Prelude.Assoc
( Lookup,
  OnceMore (..),
  assocRename,
  assocPush,
  assocGather,
  assocBy,
  assocOnce,
  assocMore,
  assocExist,

  namedMapM,
  lookupSatisfy,
  lookupMap,
  subassoc,
) where

import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Koshucode.Baala.Base.Prelude.Class as B

type Lookup a = String -> Maybe a

data OnceMore a
    = Once a
    | More [a]
      deriving (Show, Eq, Ord)

push :: a -> B.Map (OnceMore a)
push x (Once x2) = More [x, x2]
push x (More xs) = More (x : xs)

assocRename :: (Eq k) => k -> k -> B.Map [(k, a)]
assocRename new old = map r where
    r (k, x) | k == old  = (new, x)
             | otherwise = (k, x)

assocPush :: (Eq k) => k -> a -> B.Map [(k, OnceMore a)]
assocPush k x = loop where
    loop [] = [(k, Once x)]
    loop (p@(k2, x2) : xs)
        | k2 == k   = (k, push x x2) : xs
        | otherwise = p : loop xs

assocGather :: (Eq k) => [(k, a)] -> [(k, OnceMore a)]
assocGather [] = []
assocGather ((k, a) : xs) = assocPush k a $ assocGather xs

-- assocBy (`lookup` [('a', "A")]) "-" "apple banana cocoa"
-- assocBy (`lookup` [('a', "A")]) "-" "bbbac"
-- assocBy (`lookup` [('a', "A")]) "-" ""

assocBy :: (a -> Maybe k) -> k -> [a] -> [(k, [a])]
assocBy p k0 = loop k0 [] where
    loop k1 ys [] = [(k1, reverse ys)]
    loop k1 ys (x : xs) =
        case p x of
          Just k2 -> (k1, reverse ys) : loop k2 [] xs
          Nothing -> loop k1 (x : ys) xs

assocOnce :: [(k, OnceMore a)] -> [(k, a)]
assocOnce = loop where
    loop [] = []
    loop ((k, Once x) : xs) = (k, x) : loop xs
    loop (_           : xs) =          loop xs

assocMore :: [(k, OnceMore a)] -> [(k, [a])]
assocMore = loop where
    loop [] = []
    loop ((k, More x) : xs) = (k, x) : loop xs
    loop (_           : xs) =          loop xs

assocExist :: (Eq k) => k -> [(k, a)] -> Bool
assocExist k a = Maybe.isJust $ lookup k a

namedMapM :: (Monad m) => (b -> m c) -> (a, b) -> m (a, c)
namedMapM f (a, b) =
    do c <- f b
       return (a, c)

lookupSatisfy :: a -> [(a -> Bool, b)] -> Maybe b
lookupSatisfy x = loop where
    loop [] = Nothing
    loop ((p, v) : ps)
        | p x = Just v
        | otherwise = loop ps

lookupMap :: (Ord k) => k -> Map.Map k a -> Maybe a
lookupMap = Map.lookup

subassoc :: (Eq a) => [a] -> [(a, b)] -> [(a, b)]
subassoc xs = loop where
    loop [] = []
    loop (kv@(key, _) : kvs)
        | key `elem` xs = kv : loop kvs
        | otherwise     =      loop kvs

