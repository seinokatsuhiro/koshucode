{-# OPTIONS_GHC -Wall #-}

{-| Functions for changes between two datasets. -}

module Koshucode.Baala.Toolkit.Library.Change
(
-- * Minus
  minusInput,
  minusInputJudge,
  minusJudge,

-- * Update
  updateInput,
  updateJudge

-- * Changeset
-- $Changeset
) where

import qualified Data.Set as S

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Vanilla

import Koshucode.Baala.Toolkit.Library.Input
import Koshucode.Baala.Toolkit.Library.Comment



-- ----------------------  Minus

{-| Calculate and write judges /B/ minus /A/.  -}
minusInput
    :: Input   -- ^ /B:/ Source of base section
    -> Input   -- ^ /A:/ Source of altered section
    -> IO ()
minusInput inputA inputB =
    do js <- minusInputJudge inputA inputB
       putStr . unlines . texts $ minusHead inputA inputB
       putStrLn ""
       putJudges js

minusInputJudge :: Input -> Input -> IO ([Judge Val])
minusInputJudge inputA inputB =
    do [textA, textB] <- readInputs [inputA, inputB]
       return $ readJudge textA `minusJudge` readJudge textB

minusJudge :: (Ord v) => [Judge v] -> [Judge v] -> [Judge v]
minusJudge judA judB = map denyJudge judC ++ judD where
    setA = S.fromList judA
    setB = S.fromList judB
    judC = S.toList $ setB `S.difference` setA
    judD = S.toList $ setA `S.difference` setB

minusHead :: Input -> Input -> CommentDoc
minusHead inputA inputB =
    CommentDoc
    [ CommentSec "DATASETS"
      [ "There are changes C when altering dataset B into A." 
      , ""
      , "  B (base)    : " ++ inputText inputA
      , "  A (altered) : " ++ inputText inputB
      , "  C (change)  : A - B" ]
    , CommentSec "UPDATE"
      [ "Dataset A is obtained by updating B by C."
      , "Please execute: koshu-change B -u C" ]]



-- ----------------------  Update

{-| Calculate and write judges /B/ update by /C/.  -}
updateInput
    :: Input   -- ^ /B:/ Source of base section
    -> Input   -- ^ /C:/ Source of change section
    -> IO ()   -- ^ 
updateInput inputB inputC =
    do [textB, textC] <- readInputs [inputB, inputC]
       putStr . unlines . texts $ updateHead inputB inputC
       putStrLn ""
       putJudges $ readJudge textB `updateJudge` readJudge textC

updateJudge :: (Ord v) => [Judge v] -> [Judge v] -> [Judge v]
updateJudge judB judC = judA where
    setB = S.fromList $ judB
    denC = S.fromList $ map affirmJudge $ filter isDenied judC
    affC = S.fromList $ filter isAffirmed judC
    judA = S.toList $ setB `S.difference` denC `S.union` affC

updateHead :: Input -> Input -> CommentDoc
updateHead inputB inputC =
    CommentDoc
    [ CommentSec "DATASETS"
      [ "Updating dataset B by C, altered dataset A is obtained." 
      , ""
      , "  B (base)    : " ++ inputText inputB
      , "  C (change)  : " ++ inputText inputC
      , "  A (altered) : B + C" ]]



-- ----------------------
{- $Changeset

   Rules for calculating changeset C = A - B.
   Altering dataset B into A, changeset C
   is calculated by the following rules.
   
   * Affirmed judge is in C if and only if
     the judge is not in A, and is in B.
   
   * Denied judge is in C if and only if
     the judge is not in B, and is in A.
   
   * Judge is not in C otherwise.

-}

