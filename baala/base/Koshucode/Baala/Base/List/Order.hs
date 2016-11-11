{-# OPTIONS_GHC -Wall #-}

-- | Sort by ascending order or descending order.

module Koshucode.Baala.Base.List.Order
  ( -- * Data type
    OrderCap (..),
    orderingCap,
    sortByName,
    sortWith,

    -- * Rank
    Ranking,
    sortByNameNumbering,
    sortByNameDenseRank,
    sortByNameGapRank,
    denseRankFrom,
    gapRankFrom,
  ) where

import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.List.Snip    as B

-- | Data for indicating order.
data OrderCap a
    = Asc a     -- ^ Ascending order
    | Desc a    -- ^ Descending order
      deriving (Show, Eq)

instance (Ord a) => Ord (OrderCap a) where
    compare (Asc  x) (Asc  y) = compare x y
    compare (Desc x) (Desc y) = compare y x
    compare (Asc  _) (Desc _) = LT
    compare (Desc _) (Asc  _) = GT

type OrderCapping a = a -> OrderCap a

-- | Select order cap for ordering value.
--   'GT' and 'EQ' mean 'Asc', 'LT' means 'Desc'.
orderingCap :: (Ordering, a) -> OrderCap a
orderingCap (LT, a) = Desc a
orderingCap (_ , a) = Asc  a

cap :: OrderCap a -> OrderCapping b
cap (Asc  _) = Asc
cap (Desc _) = Desc

-- | Extract content from order cap.
uncap :: OrderCap a -> a
uncap (Asc  a) = a
uncap (Desc a) = a

-- | Sort by capped name.
--
--   >>> sortByName (words "a b c") [Asc "b"] [[1,3,3], [1,2,3], [1,1,3 :: Int]]
--   [[1,1,3], [1,2,3], [1,3,3]]
--
sortByName :: (Ord a, Eq n) => [OrderCap n] -> [n] -> O.Map [[a]]
sortByName ords ns = map snd . sortByNameOrder ords ns

sortByNameOrder :: (Ord a, Eq n) => [OrderCap n] -> [n] -> [[a]] -> [([OrderCap a], [a])]
sortByNameOrder ords ns xs = sortBy ords2 xs2 where
    ords2  = map cap   ords
    ns2    = map uncap ords
    p      = ns2 `B.snipIndex` ns
    xs2    = map f xs
    f x    = (B.snipFrom p x, x)

sortBy :: (Ord a, Ord b) => [OrderCapping a] -> [([a], b)] -> [([OrderCap a], b)]
sortBy ords = B.sort . B.mapFstTo (caps ords)

caps :: [OrderCapping a] -> [a] -> [OrderCap a]
caps ords = zipWith ($) (ords ++ repeat Asc)

-- | Sort elements by function result.
--
--   >>> sortWith length $ words "aaa b ccc dd"
--   ["b", "dd", "aaa", "ccc"]
--
sortWith :: (Ord a, Ord b) => (a -> b) -> O.Map [a]
sortWith f = map snd . B.sort . map g where
    g x = (f x, x)


-- ----------------------  Rank

-- | Ranking function.
type Ranking n a
    = Int               -- ^ Number of the top rank.
    -> [OrderCap n]     -- ^ Ranking specification.
    -> [n]              -- ^ Term names.
    -> [[a]]            -- ^ List of tuples.
    -> ([Int], [[a]])   -- ^ Ranks and sorted data.

-- | Sort and numbering like 1234.
sortByNameNumbering :: (Ord a, Eq n) => Ranking n a
sortByNameNumbering i ords ns = number . sortByName ords ns where
    number xs = (take (length xs) [i..], xs)

-- | Sort and rank like 1223.
--
--   >>> sortByNameDenseRank 1 [Asc "a"] ["a", "b"] [["b", "y"], ["a", "x"], ["a", "y"]]
--   ([1,1,2], [["a","x"], ["a","y"], ["b","y"]])
--
--   >>> sortByNameDenseRank 0 [Asc "b"] ["a", "b"] [["b", "y"], ["a", "x"], ["a", "y"]]
--   ([0,1,1], [["a","x"], ["a","y"], ["b","y"]])
--
--   >>> sortByNameDenseRank 0 [Desc "a"] ["a", "b"] [["b", "y"], ["a", "x"], ["a", "y"]]
--   ([0,1,1], [["b","y"], ["a","x"], ["a","y"]])
--
sortByNameDenseRank :: (Ord a, Eq n) => Ranking n a
sortByNameDenseRank i = sortByNameWithRank $ denseRankFrom i

-- | Sort and rank like 1224.
sortByNameGapRank :: (Ord a, Eq n) => Ranking n a
sortByNameGapRank i = sortByNameWithRank $ gapRankFrom i

sortByNameWithRank :: (Ord a, Eq n) => ([[OrderCap a]] -> [r]) -> [OrderCap n] -> [n] -> [[a]] -> ([r], [[a]])
sortByNameWithRank ranking ords ns xs = (rank, xs3) where
    xs2  = sortByNameOrder ords ns xs
    ord2 = map fst xs2
    xs3  = map snd xs2
    rank = ranking ord2

-- | 1223 ranking start with given number.
--
--   >>> denseRankFrom (1 :: Int) "abbc"
--   [1,2,2,3]
--
denseRankFrom :: (Ord a, Integral r) => r -> [a] -> [r]
denseRankFrom start xs = rank xs start where
    rank []  _ = []
    rank [_] r = [r]
    rank (x : ys@(y : _)) r
        | x == y    = r : rank ys r
        | otherwise = r : rank ys (r + 1)

-- | 1224 ranking start with given number.
--
--   >>> gapRankFrom (1 :: Int) "abbc"
--   [1,2,2,4]
--
gapRankFrom :: (Ord a, Integral r) => r -> [a] -> [r]
gapRankFrom start xs = rank xs start start where
    rank []  _ _ = []
    rank [_] _ r = [r]
    rank (x : ys@(y : _)) n r
        | x == y    = r : rank ys (n + 1) r
        | otherwise = r : rank ys (n + 1) (n + 1)

