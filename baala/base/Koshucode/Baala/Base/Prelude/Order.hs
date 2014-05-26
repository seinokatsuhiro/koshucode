{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Order
( OrderCap (..),
  Ranking,
  sortByName,
  sortByNameDenseRank,
  sortByNameGapRank,
  sortByNameNumbering,
  denseRankFrom,
  gapRankFrom,
) where

import qualified Koshucode.Baala.Base.Prelude.Class   as B
import qualified Koshucode.Baala.Base.Prelude.Import  as B
import qualified Koshucode.Baala.Base.Prelude.Snip    as B
import qualified Koshucode.Baala.Base.Prelude.Utility as B


-- ----------------------  Order

data OrderCap a
    = Asc a | Desc a
      deriving (Show, Eq)

instance (Ord a) => Ord (OrderCap a) where
    compare (Asc  x) (Asc  y) = compare x y
    compare (Desc x) (Desc y) = compare y x
    compare (Asc  _) (Desc _) = LT
    compare (Desc _) (Asc  _) = GT

type OrderCapSign a = a -> OrderCap a

sign :: OrderCap a -> OrderCapSign b
sign (Asc  _) = Asc
sign (Desc _) = Desc

uncap :: OrderCap a -> a
uncap (Asc  x) = x
uncap (Desc x) = x

caps :: [OrderCapSign a] -> [a] -> [OrderCap a]
caps ords = zipWith ($) (ords ++ repeat Asc)

sortBy :: (Ord a, Ord b) =>
  [OrderCapSign a] -> [([a], b)] -> [([OrderCap a], b)]
sortBy ords = B.sort . B.mapFstTo (caps ords)

sortByName :: (Ord a, Eq n) => [OrderCap n] -> [n] -> B.Map [[a]]
sortByName ords ns = map snd . sortByNameOrder ords ns

type Ranking n a = Int -> [OrderCap n] -> [n] -> [[a]] -> ([Int], [[a]])

sortByNameGapRank :: (Ord a, Eq n) => Ranking n a
sortByNameGapRank i = sortByNameWithRank (gapRankFrom i)

sortByNameDenseRank :: (Ord a, Eq n) => Ranking n a
sortByNameDenseRank i = sortByNameWithRank (denseRankFrom i)

sortByNameNumbering :: (Ord a, Eq n) => Ranking n a
sortByNameNumbering i ords ns = number . sortByName ords ns where
    number xs = (take (length xs) [i..], xs)

sortByNameWithRank :: (Ord a, Eq n) => ([[OrderCap a]] -> [r]) -> [OrderCap n] -> [n] -> [[a]] -> ([r], [[a]])
sortByNameWithRank ranking ords ns xs = (rank, xs3) where
    xs2  = sortByNameOrder ords ns xs
    ord2 = map fst xs2
    xs3  = map snd xs2
    rank = ranking ord2

sortByNameOrder :: (Ord a, Eq n) => [OrderCap n] -> [n] -> [[a]] -> [([OrderCap a], [a])]
sortByNameOrder ords ns xs = sortBy ords2 xs2 where
    ords2 = map sign  ords
    ns2   = map uncap ords
    p     = ns2 `B.snipIndex` ns
    xs2   = map f xs
    f x   = (B.snipFrom p x, x)

-- 1223 ranking
denseRankFrom :: (Ord a, Integral r) => r -> [a] -> [r]
denseRankFrom start xs = rank xs start where
    rank []  _ = []
    rank [_] r = [r]
    rank (x : ys@(y : _)) r
        | x == y    = r : rank ys r
        | otherwise = r : rank ys (r + 1)

-- 1224 ranking
gapRankFrom :: (Ord a, Integral r) => r -> [a] -> [r]
gapRankFrom start xs = rank xs start start where
    rank []  _ _ = []
    rank [_] _ r = [r]
    rank (x : ys@(y : _)) n r
        | x == y    = r : rank ys (n + 1) r
        | otherwise = r : rank ys (n + 1) (n + 1)

-- sortByName [Asc "b"] (words "a b c") [[1,3,3], [1,2,3], [1,1,3]]

-- orders :: String -> [OrderCap [Char]]
-- orders ns = map ord ns2 where
--     ns2 = words ns
--     ord (      '/' : n) = Asc  $ '/' : n
--     ord ('+' : '/' : n) = Asc  $ '/' : n
--     ord ('-' : '/' : n) = Desc $ '/' : n
--     ord n               = error $ "unknown name: " ++ n

