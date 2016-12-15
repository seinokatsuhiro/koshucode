{-# OPTIONS_GHC -Wall #-}

-- | Dataset as a set of judges.
--
--   Dataset is like a bridge of judges and relations.
--   We can get a relation from a dataset,
--   that dataset is build from judges.

module Koshucode.Baala.Data.Decode.Dataset
  ( Dataset,
    datasetClasses,
    dataset,
    datasetAdd,
  ) where

import qualified Data.Map.Strict                     as Ms
import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Type                as T


-- | Dataset is a set of judges.
data Dataset c = Dataset (Ms.Map T.JudgeClass [[S.Term c]])

instance Show (Dataset c) where
    show = showDataset

showDataset :: Dataset c -> String
showDataset (Dataset ds) =
    "Dataset { " ++ B.intercalate " | " (desc <$> Ms.assocs ds) ++ " }"
        where desc (cl, ts) = let ns  = B.uniqueConcat (fst O.<$$> ts)
                                  s   = unwords (S.termNameString <$> ns)
                                  n   = show (length ts)
                              in n ++ " * " ++ cl ++ " " ++ s

-- | Dataset that has no judges.
instance B.Default (Dataset c) where
    def = Dataset Ms.empty

instance T.SelectRel Dataset where
    selectRel = datasetSelect B.def

-- | Retrieve judgement class and its term names.
datasetClasses :: Dataset c -> Ms.Map T.JudgeClass [S.TermName]
datasetClasses (Dataset ds) = Ms.map termName ds where
    termName ts = B.uniqueConcat (fst O.<$$> ts)

-- | Gather judges into a dataset.
dataset :: [T.Judge c] -> Dataset c
dataset js = datasetAdd js B.def

-- | Add judges to dataset.
datasetAdd :: [T.Judge c] -> O.Map (Dataset c)
datasetAdd js ds = foldr addJudge ds js

-- | Add a judge to dataset.
addJudge :: T.Judge c -> O.Map (Dataset c)
addJudge (T.JudgeAffirm cl xs) (Dataset ds1) = Dataset ds2 where
    ds2 = Ms.insertWith add cl [xs] ds1
    add new old = new ++ old
addJudge _ _ = undefined

-- | Select relation from dataset.
--   If a given term is not in judges, first argument is used.
datasetSelect :: (Ord c) => c -> Dataset c -> T.RelSelect c
datasetSelect filler (Dataset ds) cl ns = T.Rel he bo where
    he = T.headFrom ns
    bo = case Ms.lookup cl ds of
           Just set -> B.unique (pickContents filler ns <$> set)
           Nothing  -> []

pickContents :: c -> [S.TermName] -> [S.Term c] -> [c]
pickContents filler ns ts = map pick ns where
    pick n = B.fromMaybe filler $ lookup n ts

