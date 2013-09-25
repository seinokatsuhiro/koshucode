{-# OPTIONS_GHC -Wall #-}

{-| Utilities for association lists. -}

module Koshucode.Baala.Base.Prelude.Assoc
( OnceMore (..),
  assocRename,
  assocPush,
  assocGather,
  assocBy,
  assocOnce,
  assocMore,
) where

import qualified Koshucode.Baala.Base.Prelude.Class as B

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
assocBy :: (a -> Maybe k) -> k -> [a] -> [(k, [a])]
assocBy p k0 = margin where
    margin [] = []
    margin (x : xs) =
        case p x of
          Just k2 -> loop k2 [] xs
          Nothing -> loop k0 [x] xs

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

