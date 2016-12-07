{-# OPTIONS_GHC -Wall #-}

-- | Dataset as a set of judges.
--
--   Dataset is like a bridge of judges and relations.
--   We can get a relation from a dataset,
--   that dataset is build from judges.

module Koshucode.Baala.Data.Type.Rel.Dataset
  ( Dataset,
    dataset,
    datasetAdd,
  ) where

import qualified Data.Map                            as Map
import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Data.Type.Judge     as D
import qualified Koshucode.Baala.Data.Type.Rel.Head  as D
import qualified Koshucode.Baala.Data.Type.Rel.Rel   as D


-- | Dataset is a set of judges.
data Dataset c = Dataset (Map.Map D.JudgeClass [[S.Term c]])

-- | Dataset that has no judges.
instance B.Default (Dataset c) where
    def = Dataset Map.empty

instance D.SelectRel Dataset where
    selectRel = datasetSelect B.def

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
--   If a given term is not in judges, first argument is used.
datasetSelect :: (Ord c) => c -> Dataset c -> D.RelSelect c
datasetSelect filler (Dataset ds) cl ns = D.Rel he bo where
    he = D.headFrom ns
    bo = case Map.lookup cl ds of
           Just set -> B.unique (pickContents filler ns <$> set)
           Nothing  -> []

pickContents :: c -> [S.TermName] -> [S.Term c] -> [c]
pickContents filler ns ts = map pick ns where
    pick n = B.fromMaybe filler $ lookup n ts

