{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Judgement writer.

module Koshucode.Baala.Writer.Judge
  ( -- * Writer
    putJudges, putJudgesWith, hPutJudgesWith,
    judgesMixes,
    judgesCountMix,
    -- * Counter
    JudgeCount, JudgeCountMix,
    judgeCount, judgeCountMix, judgeSummary,
  ) where

import qualified Data.Map.Strict                     as Ms
import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.System              as O
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Type                as T
import qualified Koshucode.Baala.Core                as C


-- ----------------------  Writer

-- | Print list of judgements.
putJudges :: (Show c, B.MixEncode c) => [T.Judge c] -> IO ()
putJudges js =
    do _ <- putJudgesWith (O.exitCode 0) js
       return ()

-- | `B.stdout` version of `hPutJudgesWith`.
putJudgesWith :: (Show c, B.MixEncode c) => B.ExitCode -> [T.Judge c] -> IO B.ExitCode
putJudgesWith = hPutJudgesWith B.stdout B.def

-- | Print list of judges.
hPutJudgesWith :: (B.MixEncode c) => C.ResultWriterJudge c
hPutJudgesWith h result status js =
    do let (mx, cnt, tab) = judgesCountMix result B.mixEncode js $ judgeCountMix []
       B.hPutMix T.judgeBreak h mx
       B.hPutMix B.crlfBreak h $ judgeSummary status (cnt, tab)
       return status

-- | Edit judgements to mix text.
judgesMixes :: forall a c. (T.GetClass a) =>
    C.Result c -> (a -> B.MixText) -> JudgeCount -> [a] -> [Either JudgeCount B.MixText]
judgesMixes result mixer (c0, tab0) = loop c0 tab0 where
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

    mod25 n       = n `mod` measure == 0
    mod5  n       = n `mod` gutter  == 0
    gutter        = C.resultGutter  result
    measure       = C.resultMeasure result

    total    n    = B.mixLine (B.mixString "*** " O.++ B.mixString (count n))
    progress n    = B.mixLine (B.mixString "*** " O.++ B.mixShow n)

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

when :: (Monoid a) => a -> Bool -> a
when a True  = a
when _ False = mempty

-- | Edit judgements to mix text.
judgesCountMix :: forall c.
    C.Result c -> (T.Judge c -> B.MixText) -> [T.Judge c] -> O.Map JudgeCountMix
judgesCountMix result writer = loop where
    loop (j : js) cnt  = loop js $ put j cnt
    loop [] (mx, c, tab) =
        let mx' = (B.mixHard `when` (c > 0)) O.++ B.mixLine (total c)
        in (mx O.++ mx', c, tab)

    put :: T.Judge c -> O.Map JudgeCountMix
    put judge (mx, c, tab) =
        let c'   = c + 1
            cls  = T.getClass judge
            mx'  = gutterMix c O.++ B.mixLine (writer judge)
            tab' = Ms.alter inc cls tab
        in (mx O.++ mx', c', tab')

    gutterMix c | mod5 c && c > 0  = B.mixLine (progress c `when` mod25 c)
                | otherwise        = B.mixEmpty

    mod25 n       = n `mod` measure == 0
    mod5  n       = n `mod` gutter  == 0
    gutter        = C.resultGutter  result
    measure       = C.resultMeasure result

    total    n    = B.mixLine (B.mixString "*** " O.++ B.mixString (count n))
    progress n    = B.mixLine (B.mixString "*** " O.++ B.mixShow n)

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1


-- ----------------------  Counter

-- | Total and per-judgement counter.
type JudgeCount = (Int, Ms.Map S.JudgeClass Int)

-- | Mix text and judgement counter.
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

