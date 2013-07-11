{-# OPTIONS_GHC -Wall #-}

-- | RDF-to-Koshucode conversion

module Koshucode.Baala.Toolkit.Library.Change
(
-- * Input
  Input (..)
, inputText
, readInput
, readInputs
, readJudge

-- * Output
, putCommentLines

-- * Minus
, minusInput
, minusInputJudge
, minusJudge

-- * Changeset
-- $Changeset
) where

import qualified Data.Set as S

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Section as Sec
import Koshucode.Baala.Vanilla


-- ----------------------  Input

data Input = Stdin | File FilePath
             deriving (Show, Eq)

inputText :: Input -> String
inputText (Stdin)  = "<stdin>"
inputText (File p) = p

readInput :: Input -> IO String
readInput (Stdin)  = getContents
readInput (File p) = readFile p

readInputs :: [Input] -> IO [String]
readInputs = mapM readInput

readJudge :: String -> [Judge Val]
readJudge s =
    let root = emptySection :: Section Val
    in case sectionRead root s of
         Right sec -> sectionJudge sec
         Left _    -> []



-- ----------------------  Output

putCommentLines :: [String] -> IO ()
putCommentLines = putStr . unlines . comment

comment :: Map [String]
comment xs = xs3 where
    xs3 = "** -*- koshu -*-" : xs2 ++ ["**", ""]
    xs2 = map ("**  " ++) $ "" : xs



-- ----------------------  Minus

minusInput :: Input -> Input -> IO ()
minusInput inputA inputB =
    do minus <- minusInputJudge inputA inputB
       putCommentLines h
       putJudges minus
    where
      h = [ "DATASETS"
          , "  There are changes C when altering dataset B into A."
          , ""
          , "  B (base)    : " ++ inputText inputA
          , "  A (altered) : " ++ inputText inputB
          , "  C (change)  : A - B"
          , ""
          , "UPDATE"
          , "  Dataset A is obtained by updating B by C."
          , "  Please execute: koshu-change B -u C"
          ]

minusInputJudge :: Input -> Input -> IO ([Judge Val])
minusInputJudge inputA inputB =
    do [textA, textB] <- readInputs [inputA, inputB]
       return $ readJudge textA `minusJudge` readJudge textB

minusJudge :: (Ord v) => [Judge v] -> [Judge v] -> [Judge v]
minusJudge judA judB = map denyJudge judgeC ++ judgeD where
    setA = S.fromList judA
    setB = S.fromList judB
    judgeC = S.toList $ setB `S.difference` setA
    judgeD = S.toList $ setA `S.difference` setB


-- ----------------------
{- $Changeset

Rules for calculating changeset C = A - B.
Altering dataset B into A, changeset C
is calculated by the following rules.

* Affirmed judge is in C if and only if the judge is not in A, and is in B.

* Denied judge is in C if and only if the judge is not in B, and is in A.

* Judge is not in C otherwise.

-}

