{-# OPTIONS_GHC -Wall #-}

-- | Dataset as a set of judges.
--
--   Dataset is like a bridge of judges and relations.
--   We can get a relation from a dataset,
--   that dataset is build from judges.

module Koshucode.Baala.Core.Relmap.Dataset
( Dataset
, emptyDataset
, dataset
, addJudges
, selectRelation
) where

import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Content

-- | Dataset is a set of judges.
data Dataset c = Dataset (Map.Map B.Relsign [B.Relarg c])

-- | Dataset that has no judges
emptyDataset :: Dataset c
emptyDataset = Dataset Map.empty

-- | Gather judges into a dataset
dataset :: [B.Judge c] -> Dataset c
dataset js = addJudges js emptyDataset

-- | Add judges to dataset.
addJudges :: [B.Judge c] -> Dataset c -> Dataset c
addJudges js ds1 = foldr addJudge ds1 js

-- | Add a judge to dataset.
addJudge :: B.Judge c -> Dataset c -> Dataset c
addJudge (B.Judge True sign xs) (Dataset ds1) = Dataset ds2 where
    ds2 = Map.insertWith add sign [xs] ds1
    add new old = new ++ old
addJudge (B.Judge False _ _) _ = undefined

-- | Select relation from dataset.
--   If a giving term is not in judges, 'CNil' sign is used.
selectRelation
    :: (Ord c, CNil c)
    => Dataset c   -- ^ Dataset
    -> B.Relsign   -- ^ Relsign to select
    -> [String]    -- ^ List of term names
    -> B.Rel c     -- ^ Selected relation
selectRelation (Dataset m) sign ns = B.Rel h1 b1 where
    h1 = B.Relhead $ map B.Term ns
    b1 = case Map.lookup sign m of
      Just args -> B.unique $ map (subarg ns) args
      Nothing   -> []

subarg :: (CNil c) => [String] -> B.Relarg c -> [c]
subarg ns arg = map pick ns where
    pick n = Maybe.fromMaybe nil $ lookup n arg

