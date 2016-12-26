{-# OPTIONS_GHC -Wall #-}

-- | Functions for changes between two datasets.

module Koshucode.Baala.Toolkit.Library.Change
  (
    -- * Minus
    minusInput,
    minusInputJudge,
    minusJudge,
  
    -- * Update
    updateInput,
    updateJudge,
  
    -- * Changeset
    -- $Changeset
  ) where

import qualified Data.Set                              as S
import qualified Koshucode.Baala.System                as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Type                  as T
import qualified Koshucode.Baala.Data                  as D
import qualified Koshucode.Baala.Writer                as W
import qualified Koshucode.Baala.Toolkit.Library.Input as L



-- ----------------------  Minus

-- | Read, calculate, and write judges /B/ minus /A/.
minusInput
    :: L.Input   -- ^ /B:/ Source of base section
    -> L.Input   -- ^ /A:/ Source of altered section
    -> IO B.ExitCode
minusInput inputA inputB =
    do js <- minusInputJudge inputA inputB
       putStr . unlines . B.texts $ minusHead inputA inputB
       putStrLn ""
       W.putJudgesWith (O.exitCode 0) js

-- | Read and calculate judges subtraction.
minusInputJudge :: L.Input -> L.Input -> IO [D.JudgeC]
minusInputJudge inputA inputB =
    do [textA, textB] <- L.readInputs [inputA, inputB]
       return $ L.readJudge textA `minusJudge` L.readJudge textB

-- | Calculate subtraction of judges.
minusJudge :: (Ord c) => [T.Judge c] -> [T.Judge c] -> [T.Judge c]
minusJudge judA judB = map T.denyJudge judC ++ judD where
    setA = S.fromList judA
    setB = S.fromList judB
    judC = S.toList $ setB `S.difference` setA
    judD = S.toList $ setA `S.difference` setB

minusHead :: L.Input -> L.Input -> B.CommentDoc
minusHead inputA inputB =
    B.CommentDoc
    [ B.CommentSec "DATASETS"
      [ "There are changes C when altering dataset B into A." 
      , ""
      , "  B (base)    : " ++ L.inputText inputB
      , "  A (altered) : " ++ L.inputText inputA
      , "  C (change)  : A - B" ]
    , B.CommentSec "UPDATE"
      [ "Dataset A is obtained by updating B by C."
      , "Please execute: koshu-change B -u C" ]]



-- ----------------------  Update

-- | Calculate and write judges /B/ update by /C/.
updateInput
    :: L.Input   -- ^ /B:/ Source of base section
    -> L.Input   -- ^ /C:/ Source of change section
    -> IO B.ExitCode  -- ^ 
updateInput inputB inputC =
    do [textB, textC] <- L.readInputs [inputB, inputC]
       putStr . unlines . B.texts $ updateHead inputB inputC
       putStrLn ""
       W.putJudgesWith (O.exitCode 0) $ L.readJudge textB `updateJudge` L.readJudge textC

-- | Apply change judges.
updateJudge :: (Ord c) => [T.Judge c] -> [T.Judge c] -> [T.Judge c]
updateJudge judB judC = judA where
    setB = S.fromList $ judB
    denC = S.fromList $ map T.affirmJudge $ filter T.isDenial judC
    affC = S.fromList $ filter T.isAffirmative judC
    judA = S.toList $ setB `S.difference` denC `S.union` affC

updateHead :: L.Input -> L.Input -> B.CommentDoc
updateHead inputB inputC =
    B.CommentDoc
    [ B.CommentSec "DATASETS"
      [ "Updating dataset B by C, altered dataset A is obtained." 
      , ""
      , "  B (base)    : " ++ L.inputText inputB
      , "  C (change)  : " ++ L.inputText inputC
      , "  A (altered) : B + C" ]]


-- ----------------------
-- $Changeset
--
--  Rules for calculating changeset C = A - B.
--  Altering dataset B into A, changeset C
--  is calculated by the following rules.
--  
--  * Affirmed judge is in C if and only if
--    the judge is not in A, and is in B.
--  
--  * Denied judge is in C if and only if
--    the judge is not in B, and is in A.
--  
--  * Judge is not in C otherwise.
--

