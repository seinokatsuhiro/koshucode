{-# OPTIONS_GHC -Wall #-}

-- | Dataset as a set of judges.
--
--   Dataset is like a bridge of judges and relations.
--   We can get a relation from a dataset,
--   that dataset is build from judges.

module Koshucode.Baala.Base.Relmap.Dataset
( Dataset
, emptyDataset
, dataset
, addJudges
, selectRelation
) where

import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Content.Class

-- | Dataset is a set of judges.
data Dataset v = Dataset (Map.Map Relsign [Relarg v])

-- | Dataset that has no judges
emptyDataset :: Dataset v
emptyDataset = Dataset Map.empty

-- | Gather judges into a dataset
dataset :: [Judge v] -> Dataset v
dataset js = addJudges js emptyDataset

-- | Add judges to dataset.
addJudges :: [Judge v] -> Dataset v -> Dataset v
addJudges js ds1 = foldr addJudge ds1 js

-- | Add a judge to dataset.
addJudge :: Judge v -> Dataset v -> Dataset v
addJudge (Judge True sign xs) (Dataset ds1) = Dataset ds2 where
    ds2 = Map.insertWith add sign [xs] ds1
    add new old = new ++ old
addJudge (Judge False _ _) _ = undefined

-- | Select relation from dataset.
--   If a giving term is not in judges, 'Nil' sign is used.
selectRelation
    :: (Ord v, Nil v)
    => Dataset v   -- ^ Dataset
    -> Relsign     -- ^ Relsign to select
    -> [String]    -- ^ List of term names
    -> Rel v       -- ^ Selected relation
selectRelation (Dataset m) sign ns = Rel h1 b1 where
    h1 = Relhead $ map Term ns
    b1 = case Map.lookup sign m of
      Just args -> unique $ map (subarg ns) args
      Nothing   -> []

subarg :: (Nil v) => [String] -> Relarg v -> [v]
subarg ns arg = map pick ns where
    pick n = Maybe.fromMaybe nil $ lookup n arg

