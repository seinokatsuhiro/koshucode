{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.DepRank
( DepRank (..),
  DepRankMap,
  depRankFromPairs,
  depRankList,
  depRankUpdateAll,
  depRankUpdate,
) where

import qualified Data.Map             as Map
import qualified Koshucode.Baala.Base as B

-- | Dependent ranking data.
data DepRank a
    = DepRank [a] (DepRankMap a)
      deriving (Show, Eq, Ord)

type DepRankMap a = Map.Map a ([a], Maybe Int)

-- | Construct dependent ranking data.
depRankFromPairs :: (Ord a) => [(a, a)] -> DepRank a
depRankFromPairs dep = DepRank xs m where
    xs    =  Map.keys m
    m     =  Map.map f $ B.gatherToMap dep
    f ys  =  (ys, Nothing)

-- | Get dependent ranking.
depRankList :: DepRank a -> [(a, Int)]
depRankList (DepRank _ m) = B.catMaybes $ map f $ Map.assocs m where
    f (x, (_, Just r))  = Just (x, r)
    f (_, (_, Nothing)) = Nothing

-- | Update dependent rank for all elements.
depRankUpdateAll :: (Ord a) => B.Map (DepRank a)
depRankUpdateAll dr@(DepRank xs _) = foldr depRankUpdate dr xs

-- | Update dependent rank for an element.
depRankUpdate :: forall a. (Ord a) => a -> B.Map (DepRank a)
depRankUpdate x0 (DepRank xs m0) = DepRank xs $ loop x0 m0 where
    loop x m = case Map.lookup x m of
                 Nothing             ->  Map.insert x zero m
                 Just ([], Nothing)  ->  Map.update zerof x m
                 Just (ys, Nothing)  ->  rank x ys $ foldr loop m ys
                 Just (_,  Just _)   ->  m

    zero          =  p [] 0
    zerof _       =  Just zero
    up r (ys, _)  =  Just $ p ys $ r + 1
    p ys r        =  (ys, Just r)

    rank :: a -> [a] -> B.Map (DepRankMap a)
    rank x ys m = case get m `mapM` ys of
                    Nothing -> m
                    Just rs -> let ymax = maximum rs
                               in Map.update (up ymax) x m

    get :: DepRankMap a -> a -> Maybe Int
    get m y     = case Map.lookup y m of
                    Just (_, Just r) -> Just r
                    _                -> Nothing

-- dp :: DepRank Int
-- dp = depRankFromPairs [(1,0), (2,1), (3,1), (2,0)]
-- depRankList $ depRankUpdateAll dp

