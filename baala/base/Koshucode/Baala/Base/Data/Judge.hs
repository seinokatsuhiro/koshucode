{-# OPTIONS_GHC -Wall #-}

{-| Judgements: a symbolic representations of
    affirmed or denied statements. -}

module Koshucode.Baala.Base.Data.Judge
(
  -- * Datatype
  Judge (Judge),
  JudgePattern,
  abcJudge,

  -- * Logical quality
  affirm, deny,
  affirmJudge, denyJudge,
  isAffirmed, isDenied,

  -- * Writer
  putJudges,
  hPutJudges,
  judgeLines,
) where

import qualified Data.Monoid as Monoid
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified System.IO   as IO
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Data.Comment as B


-- ----------------------  Datatype

{-| Judgement on type 'c'.
 
    Judgement (or judge for short) is divided into three parts:
    logical quality, name of pattern, and argument.
    Boolean values 'True' or 'False' of logical quality
    corresponds to affirmed or denied judge.
    A name of judgement pattern represents
    certain sentence pattern that gives intepretation of data.
    Sentence pattern has placeholders filled by
    'B.Named' @c@ in argument. -} 

data Judge c = Judge Bool JudgePattern [B.Named c]
               deriving (Show)

instance (Ord c) => Eq (Judge c) where
    j1 == j2  =  compare j1 j2 == EQ
    j1 /= j2  =  compare j1 j2 /= EQ

instance (Ord c) => Ord (Judge c) where
    compare j1 j2 =
        let Judge q1 p1 xs1 = abcJudge j1
            Judge q2 p2 xs2 = abcJudge j2
        in compare p1 p2
               `Monoid.mappend` compare q1 q2
               `Monoid.mappend` compare xs1 xs2

{-| Name of judgement pattern. -}
type JudgePattern = String

-- Apply function to each values
instance Functor Judge where
    fmap f (Judge q p a) = Judge q p $ map g a
        where g (n, v) = (n, f v)

{-| >>> doc $ Judge True "P" [("/a", 10), ("/b", 20 :: Int)]
    |-- P  /a 10  /b 20 -}
instance (Ord c, B.Pretty c) => B.Pretty (Judge c) where
    doc (Judge q p a) = quality q B.<+> sign B.<+> arg a
        where
          -- Frege's judgement stroke, content line,
          -- and logical quality
          quality True  = B.doc "|--"
          quality False = B.doc "|-X"
          -- pattern
          sign | ':' `elem` p = B.docWrap "\"" "\"" p
               | otherwise    = B.doc p
          -- term name and term value
          arg ((n,v) : a2) = B.doc " " B.<> B.doc n
                             B.<+> B.doc v B.<+> arg a2
          arg [] = B.docEmpty

{-| Sort terms in alphabetical order. -}
abcJudge :: (Ord c) => B.Map (Judge c)
abcJudge (Judge q p a) = Judge q p $ List.sort a



-- ----------------------  Logical quality

{-| Construct affirmed judgement. -}
affirm :: JudgePattern -> [B.Named c] -> Judge c
affirm = Judge True

{-| Construct denied judgement. -}
deny :: JudgePattern -> [B.Named c] -> Judge c
deny = Judge False

{-| Affirm judgement, i.e., change logical quality to 'True'. -}
affirmJudge :: B.Map (Judge c)
affirmJudge (Judge _ p xs) = Judge True p xs

{-| Deny judgement, i.e., change logical quality to 'False'. -}
denyJudge   :: B.Map (Judge c)
denyJudge   (Judge _ p xs) = Judge False p xs

{-| Test that judgement is affirmd. -}
isAffirmed :: Judge c -> Bool
isAffirmed (Judge q _ _) = q

{-| Test that judgement is denied.

    >>> isDenied $ Judge True "A" []
    False  -}
isDenied   :: Judge c -> Bool
isDenied   (Judge q _ _) = not q



-- ----------------------  Writer

{-| Print judges to `IO.stdout`. -}
putJudges :: (Ord c, B.Pretty c) => Int -> [Judge c] -> IO Int
putJudges = hPutJudges IO.stdout

{-| Print judges. -}
hPutJudges :: (Ord c, B.Pretty c) => IO.Handle -> Int -> [Judge c] -> IO Int
hPutJudges h status js =
    do IO.hPutStr h $ unlines $ judgeLines label js
       return status
    where
      label | status == 0 = "SUMMARY"
            | otherwise   = "SUMMARY (VIOLATED)"

{-| Convert judgements to lines. -}
judgeLines :: (Ord c, B.Pretty c) => String -> [Judge c] -> [String]
judgeLines label = loop 0 Map.empty where
    loop n c []         =  judgeSummary label n $ Map.assocs c
    loop n c (j : js)
        | by 20         =  s : count n' : gutter ss
        | by 5          =  s : gutter ss
        | otherwise     =  s : ss
        where s         =  show $ B.doc j
              ss        =  loop n' (up c j) js
              by d      =  n' `mod` d == 0
              n'        =  n + 1

    up c (Judge _ p _)  =  Map.alter alt p c
    alt Nothing         =  Just 1
    alt (Just n)        =  Just $ n + 1

    count n             =  "**  (" ++ show n ++ " judges)"

    gutter ss@("" : _)  =  ss
    gutter ss           =  "" : ss

-- >>> putStr . unlines $ judgeSummary 10 [("A", 3), ("B", 6), ("C", 1)]
judgeSummary :: String -> Int -> [(JudgePattern, Int)] -> [String]
judgeSummary label tt ns     =  "" : B.texts summaryDoc where
    summaryDoc         =  B.CommentDoc [summary]
    summary            =  B.CommentSec label $ sumLines ++ [total tt]
    sumLines           =  map sumLine ns
    sumLine (p, n)     =  count n ++ " on " ++ p
    total n            =  count n ++ " in total"

    count 0            =  comment $ "no judges"
    count 1            =  comment $ "1 judge "
    count n            =  comment $ show n ++ " judges"
    comment j          =  B.padLeft 11 j

