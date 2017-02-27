{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Judgement writer.

module Koshucode.Baala.Writer.Judge
  ( -- * Writer
    putJudges, hPutJudges,
    mixJudgesCount,

    -- * Counter
    JudgeCount, JudgeCountMix,
    judgeCount, judgeCountMix, judgeSummary,
  ) where

import qualified Data.Map.Strict                     as Ms
import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Type                as T
import qualified Koshucode.Baala.Core                as C


-- ----------------------  Writer

{-| Print list of judgements. -}
putJudges :: (Show c, B.MixEncode c) => [T.Judge c] -> IO ()
putJudges = hPutJudges B.stdout B.def

{-| Print list of judgements. -}
hPutJudges :: (B.MixEncode c) => C.ResultWriterJudge c
hPutJudges h result js =
    do let port  = C.resultPortable result
           cnt   = judgeCount []
           ls    = mixJudgesCount port B.mixEncode js
       cnt' <- B.hPutMixEither T.judgeBreak h cnt [ls]
       B.hPutMix B.crlfBreak h $ judgeSummary (C.resultStatus port) cnt'

{-| Edit judgements to mix text. -}
mixJudgesCount :: (T.GetClass a) =>
    C.ResultPortable -> (a -> B.MixText) -> [a] -> JudgeCount -> [B.MixEither JudgeCount]
mixJudgesCount p mixer xs (c0, tab0) = loop c0 tab0 xs where
    loop c tab (j : js) = mixing c tab j js
    loop c tab [] =
        let mix = (B.mixHard `when` (c > 0)) O.++ B.mixLine (total c)
        in [Right mix, Left (c, tab)]

    mixing c tab j js =
        let !c'   = c + 1
            !tab' = Ms.alter inc (T.getClass j) tab
            mix   = gutterMix c $ B.mixLine (mixer j)
        in Right mix : loop c' tab' js

    gutterMix c m | mod5 c && c > 0  = B.mixLine (progress c `when` mod25 c) O.++ m
                  | otherwise        = m

    mod25 n       = n `mod` C.resultMeasure p == 0
    mod5  n       = n `mod` C.resultGutter  p == 0
    total    n    = B.mixLine (B.mixString "*** " O.++ B.mixString (count n))
    progress n    = B.mixLine (B.mixString "*** " O.++ B.mixShow n)

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

when :: (Monoid a) => a -> Bool -> a
when a True  = a
when _ False = mempty


-- ----------------------  Counter

-- | Total and per-judgement counter.
type JudgeCount = (Int, Ms.Map S.JudgeClass Int)

-- | Mix text and judgement counter.
{-# DEPRECATED JudgeCountMix "Use 'JudgeCount' instead." #-}
type JudgeCountMix = (B.MixText, Int, Ms.Map S.JudgeClass Int)

{-| Zero counters.

    === __Examples__

    >>> judgeCount $ words "A B C"
    (0, fromList [("A",0), ("B",0), ("C",0)])
    -}
judgeCount :: [S.JudgeClass] -> JudgeCount
judgeCount ps = (0, Ms.fromList $ zip ps $ repeat 0)

-- | Empty and zero counters.
judgeCountMix :: [S.JudgeClass] -> JudgeCountMix
judgeCountMix ps = (B.mixEmpty, 0, Ms.fromList $ zip ps $ repeat 0)

{-| Generate judgement counter comment.

    === __Examples__

    >>> B.putMix B.crlfBreak $ judgeSummary (O.exitCode 0) (10, Ms.fromList [("A", 3), ("B", 6), ("C", 1)])
    **
    **  SUMMARY
    **       3 judges on A
    **       6 judges on B
    **       1 judge  on C
    **      10 judges in total
    **
    -}
judgeSummary :: B.ExitCode -> JudgeCount -> B.MixText
judgeSummary status (_, tab) = B.mixLines (B.mix <$> B.texts sumDoc) where
    label | status == B.ExitSuccess = "SUMMARY"
          | otherwise               = "SUMMARY (VIOLATED)"

    sumDoc              = B.CommentDoc [sumSec]
    sumSec              = B.CommentSec label $ sumLines ++ [total]
    sumLines            = map sumLine $ Ms.assocs tab
    sumLine (p, n)      = countPad n ++ " on " ++ p
    total               = countPad (sumOf tab) ++ " in total"

    sumOf :: Ms.Map a Int -> Int
    sumOf = Ms.foldr (+) 0

    countPad = O.padBegin 11 . count

count :: Int -> String
count 0  = "no judges"
count 1  = "1 judge "
count n  = show n ++ " judges"

