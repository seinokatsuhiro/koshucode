{-# OPTIONS_GHC -Wall #-}

-- | Read data file.

module Koshucode.Baala.Data.Decode.Read
  ( readDataset,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Type                    as D
import qualified Koshucode.Baala.Data.Class              as D
import qualified Koshucode.Baala.Data.Decode.Term        as D
import qualified Koshucode.Baala.Data.Decode.Content     as D
import qualified Koshucode.Baala.Syntax.Pattern          as P

-- | Read file and convert to dataset.
--
--   >>> readDataset "foo.k" :: Koshucode.Baala.Base.IOAb DatasetC
--   Right Dataset { 3 * A /x /y | 2 * B /y /z | 1 * C /z }
--
readDataset :: (D.CContent c) => FilePath -> B.IOAb (D.Dataset c)
readDataset path =
    do ts' <- S.readClauseTrees path
       case ts' of
         Left a   -> return $ Left a
         Right ts -> case clauseJudges ts of
                       Left a   -> return $ Left a
                       Right js -> return $ Right $ D.dataset js

clauseJudges :: (D.CContent c) => [[S.TTree]] -> B.Ab [D.Judge c]
clauseJudges = loop D.cacheT [] where
    loop cc js ((P.L (P.TBar "|--") : P.LRaw cl : ts) : ls) =
        do (cc', j) <- D.treesJudge cc D.AssertAffirm cl ts
           loop cc' (j : js) ls
    loop _ js [] = Right js
    loop _ _ ls  = B.abortable "clause" ls $ Left $ B.abortBecause "Except judgement"

