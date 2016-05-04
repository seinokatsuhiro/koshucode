{-# OPTIONS_GHC -Wall #-}

-- | Dataset as a set of judges.
--
--   Dataset is like a bridge of judges and relations.
--   We can get a relation from a dataset,
--   that dataset is build from judges.

module Koshucode.Baala.Core.Assert.Dataset
  ( Dataset,
    datasetEmpty,
    dataset,
    datasetAdd,
    datasetSelect,
  ) where

import qualified Data.Map                     as Map
import qualified Data.Maybe                   as Maybe
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core.Relkit  as C


-- | Dataset is a set of judges.
data Dataset c = Dataset (Map.Map D.JudgePat [[S.Term c]])

-- | Dataset that has no judges.
datasetEmpty :: Dataset c
datasetEmpty = Dataset Map.empty

-- | Gather judges into a dataset.
dataset :: [D.Judge c] -> Dataset c
dataset js = datasetAdd js datasetEmpty

-- | Add judges to dataset.
datasetAdd :: [D.Judge c] -> Dataset c -> Dataset c
datasetAdd js ds1 = foldr addJudge ds1 js

-- | Add a judge to dataset.
addJudge :: D.Judge c -> Dataset c -> Dataset c
addJudge (D.JudgeAffirm sign xs) (Dataset ds1) = Dataset ds2 where
    ds2 = Map.insertWith add sign [xs] ds1
    add new old = new ++ old
addJudge _ _ = undefined

-- | Select relation from dataset.
--   If a giving term is not in judges, 'CEmpty' sign is used.
datasetSelect
    :: (Ord c, D.CEmpty c)
    => Dataset c       -- ^ Dataset
    -> C.RelSelect c   -- ^ Relation selector
datasetSelect (Dataset m) sign ns = D.Rel h1 b1 where
    h1 = D.headFrom ns
    b1 = case Map.lookup sign m of
      Just args -> B.unique $ map (subarg ns) args
      Nothing   -> []

subarg :: (D.CEmpty c) => [String] -> [S.Term c] -> [c]
subarg ns arg = map pick ns where
    pick n = Maybe.fromMaybe D.empty $ lookup n arg

