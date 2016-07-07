{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Judge
  ( -- * Writer
    putJudges, putJudgesWith, hPutJudgesWith,
    judgesCountMix,
    -- * Counter
    JudgeCount, JudgeCountMix,
    judgeCount, judgeCountMix, judgeSummary,
  ) where

import Data.Monoid ((<>))
import qualified Data.Map                            as Map
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Data                as D
import qualified Koshucode.Baala.Core                as C


-- ----------------------  Writer

putJudges :: (Show c, B.MixShortEncode c) => [D.Judge c] -> IO ()
putJudges js =
    do _ <- putJudgesWith (B.exitCode 0) js
       return ()

-- | `B.stdout` version of `hPutJudgesWith`.
putJudgesWith :: (Show c, B.MixShortEncode c) => B.ExitCode -> [D.Judge c] -> IO B.ExitCode
putJudgesWith = hPutJudgesWith B.stdout B.def

-- | Print list of judges.
hPutJudgesWith :: (B.MixShortEncode c) => C.ResultWriterJudge c
hPutJudgesWith h result status js =
    do let (mx, cnt, tab) = judgesCountMix result B.mixIdEncode js $ judgeCountMix []
       B.hPutMix D.judgeBreak h mx
       B.hPutLines h $ judgeSummary status (cnt, tab)
       return status

judgesCountMix :: forall c.
    C.Result c -> (D.Judge c -> B.MixText) -> [D.Judge c] -> B.Map JudgeCountMix
judgesCountMix result writer = loop where
    loop (j : js) cnt  = loop js $ put j cnt
    loop [] (mx, c, tab) =
        let mx' = (B.mixHard `when` (c > 0)) <> B.mixLine (total c)
        in (mx <> mx', c, tab)

    put :: D.Judge c -> B.Map JudgeCountMix
    put judge (mx, c, tab) =
        let c'   = c + 1
            cls  = D.getClass judge
            mx'  = gutterMix c <> B.mixLine (writer judge)
            tab' = Map.alter inc cls tab
        in (mx <> mx', c', tab')

    gutterMix c | mod5 c && c > 0  = B.mixLine (progress c `when` mod25 c)
                | otherwise        = B.mixEmpty

    mod25 n       = n `mod` measure == 0
    mod5  n       = n `mod` gutter  == 0
    gutter        = C.resultGutter  result
    measure       = C.resultMeasure result

    total    n    = B.mixLine (B.mixString "*** " <> B.mixString (count n))
    progress n    = B.mixLine (B.mixString "*** " <> B.mixShow n)

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

when :: (Monoid a) => a -> Bool -> a
when a True  = a
when _ False = mempty

-- ----------------------  Counter

-- | Total and per-judgement counter
type JudgeCount = (Int, Map.Map D.JudgeClass Int)
type JudgeCountMix = (B.MixText, Int, Map.Map D.JudgeClass Int)

judgeCount :: [D.JudgeClass] -> JudgeCount
judgeCount ps = (0, Map.fromList $ zip ps $ repeat 0)

judgeCountMix :: [D.JudgeClass] -> JudgeCountMix
judgeCountMix ps = (B.mixEmpty, 0, Map.fromList $ zip ps $ repeat 0)

-- B.putLines $ summaryLines 0 (10, Map.fromList [("A", 3), ("B", 6), ("C", 1)])
judgeSummary :: B.ExitCode -> JudgeCount -> [String]
judgeSummary status (_, tab) = B.texts sumDoc where
    label | status == B.ExitSuccess = "SUMMARY"
          | otherwise               = "SUMMARY (VIOLATED)"

    sumDoc              = B.CommentDoc [sumSec]
    sumSec              = B.CommentSec label $ sumLines ++ [total]
    sumLines            = map sumLine $ Map.assocs tab
    sumLine (p, n)      = countPad n ++ " on " ++ p
    total               = countPad (sumOf tab) ++ " in total"

    sumOf :: Map.Map a Int -> Int
    sumOf = Map.foldr (+) 0

    countPad = B.padLeft 11 . count

count :: Int -> String
count 0  = "no judges"
count 1  = "1 judge "
count n  = show n ++ " judges"

