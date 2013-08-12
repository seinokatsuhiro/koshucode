{-# OPTIONS_GHC -Wall #-}

{-| Judgements: a symbolic representations of
    affirmed or denied statements. -}

module Koshucode.Baala.Base.Data.Judge
(
  -- * Datatype
  Judge (Judge),
  JudgePattern,

  -- * Logical quality
  affirmJudge,
  denyJudge,
  isAffirmed,
  isDenied,

  -- * Writer
  putJudges,
  hPutJudges,
  abcJudge,
) where

import qualified Data.List as List
import qualified Data.Map  as Map
import qualified System.IO as IO
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data.Comment


-- ----------------------  Datatype

{-| Judgement on type 'c'.
 
    Judgement (or judge for short) is divided into three parts:
    logical quality, sign of relation, and argument.
    'Bool' value of logical quality corresponds to
    affirmed or denied judge.
    'String' value of sign represents certain sentence pattern
    that gives intepretation of data.
    Sentence pattern has placeholders filled by
    ('String', 'c') values of argument. --} 

data Judge c = Judge Bool JudgePattern [Named c]
               deriving (Show, Eq, Ord)

{-| Pattern of judgement. -}
type JudgePattern = String

-- Apply function to each values
instance Functor Judge where
    fmap f (Judge q s a) = Judge q s $ map g a
        where g (n, v) = (n, f v)

--  Pretty printing
instance (Ord c, Pretty c) => Pretty (Judge c) where
    doc (Judge q s a) = quality q <+> sign <+> arg a
        where
          -- Frege's judgement stroke, content line,
          -- and logical quality
          quality True  = text "|--"
          quality False = text "|-X"
          -- relsign
          sign | ':' `elem` s = docQuote $ text s
               | otherwise    = text s
          -- term name and term value
          arg ((n,v) : a2) = text " " <> text n <+> doc v <+> arg a2
          arg [] = empty



-- ----------------------  Logical quality

{-| Affirm judge, i.e., change logical quality to 'True'. -}
affirmJudge :: Map (Judge c)
affirmJudge (Judge _ p xs) = Judge True p xs

{-| Deny judge, i.e., change logical quality to 'False'. -}
denyJudge   :: Map (Judge c)
denyJudge   (Judge _ p xs) = Judge False p xs

isAffirmed :: Judge c -> Bool
isAffirmed (Judge q _ _) = q

isDenied   :: Judge c -> Bool
isDenied   (Judge q _ _) = not q



-- ----------------------  Writer

{-| Print judges. -}
putJudges :: (Ord c, Pretty c) => [Judge c] -> IO ()
putJudges = hPutJudges IO.stdout

hPutJudges :: (Ord c, Pretty c) => IO.Handle -> [Judge c] -> IO ()
hPutJudges h = IO.hPutStr h . unlines . judgeLines

{-| Convert judgements to lines. -}
judgeLines :: (Ord c, Pretty c) => [Judge c] -> [String]
judgeLines = loop zero Map.empty where
    loop n c []        =  judgeSummary n $ Map.assocs c
    loop n c (j : js)
        | grad   n'    =  s : process n' : "" : ss
        | gutter n'    =  s : "" : ss
        | otherwise    =  s : ss
        where s        =  show $ doc j
              ss       =  loop n' (up c j) js
              n'       =  n + 1

    up c (Judge _ p _) =  Map.alter upAlter p c
    upAlter Nothing    =  Just one
    upAlter (Just n)   =  Just $ n + 1

    grad   n           =  n `mod` 20  == 0
    gutter n           =  n `mod` 5   == 0

    process n          =  "**  (" ++ show n ++ " judges)"

    zero               =  0 :: Int
    one                =  1 :: Int

-- >>> putStr . unlines $ judgeSummary 10 [("A", 3), ("B", 6), ("C", 1)]
judgeSummary :: Int -> [(JudgePattern, Int)] -> [String]
judgeSummary tt ns     =  "" : texts summaryDoc where
    summaryDoc         =  CommentDoc [summary]
    summary            =  CommentSec "SUMMARY" $ sumLines ++ [total tt]
    sumLines           =  map sumLine ns
    sumLine (p, n)     =  count n ++ " on " ++ p
    total n            =  count n ++ " in total"

    count 0            =  comment $ "no judges"
    count 1            =  comment $ "1 judge "
    count n            =  comment $ show n ++ " judges"
    comment j          =  padLeft 11 j

{-| Sort terms in alphabetical order. -}
abcJudge :: (Ord c) => Map (Judge c)
abcJudge (Judge q p a) = Judge q p $ List.sort a

