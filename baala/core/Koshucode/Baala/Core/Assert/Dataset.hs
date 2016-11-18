{-# OPTIONS_GHC -Wall #-}

-- | Dataset as a set of judges.
--
--   Dataset is like a bridge of judges and relations.
--   We can get a relation from a dataset,
--   that dataset is build from judges.

module Koshucode.Baala.Core.Assert.Dataset
  ( Dataset,
    dataset,
    datasetAdd,
    datasetSelect,
  ) where

import qualified Data.Map                     as Map
import qualified Data.Maybe                   as Maybe
import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Data         as D


-- | Dataset is a set of judges.
data Dataset c = Dataset (Map.Map D.JudgeClass [[S.Term c]])

-- | Dataset that has no judges.
instance B.Default (Dataset c) where
    def = Dataset Map.empty

-- | Gather judges into a dataset.
dataset :: [D.Judge c] -> Dataset c
dataset js = datasetAdd js B.def

-- | Add judges to dataset.
datasetAdd :: [D.Judge c] -> O.Map (Dataset c)
datasetAdd js ds = foldr addJudge ds js

-- | Add a judge to dataset.
addJudge :: D.Judge c -> O.Map (Dataset c)
addJudge (D.JudgeAffirm cl xs) (Dataset ds1) = Dataset ds2 where
    ds2 = Map.insertWith add cl [xs] ds1
    add new old = new ++ old
addJudge _ _ = undefined

-- | Select relation from dataset.
--   If a giving term is not in judges, 'CEmpty' sign is used.
datasetSelect
    :: (Ord c, D.CEmpty c)
    => Dataset c       -- ^ Dataset
    -> D.RelSelect c   -- ^ Relation selector
datasetSelect (Dataset ds) cl ns = D.Rel he bo where
    he = D.headFrom ns
    bo = case Map.lookup cl ds of
           Just set -> B.unique (selectTuple ns <$> set)
           Nothing     -> []

selectTuple :: (D.CEmpty c) => [S.TermName] -> [S.Term c] -> [c]
selectTuple ns ts = map pick ns where
    pick n = Maybe.fromMaybe D.empty $ lookup n ts

