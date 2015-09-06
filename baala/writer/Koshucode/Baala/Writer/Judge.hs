{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Judge
  ( -- * Writer
    putJudges, putJudgesWith, hPutJudgesWith, hPutJudgesCount,
    -- * Counter
    JudgeCount, judgeCount, judgeSummary,
  ) where

import qualified Control.Monad                       as M
import qualified Data.Map                            as Map
import qualified System.IO                           as IO
import qualified Koshucode.Baala.Base                as B


-- ----------------------  Writer

putJudges :: (Show c, B.Write c) => [B.Judge c] -> IO ()
putJudges js =
    do _ <- putJudgesWith (B.exitCode 0) js
       return ()

-- | `B.stdout` version of `hPutJudgesWith`.
putJudgesWith :: (Show c, B.Write c) => B.ExitCode -> [B.Judge c] -> IO B.ExitCode
putJudgesWith = hPutJudgesWith B.stdout B.resultEmpty

-- | Print list of judges.
hPutJudgesWith :: (B.Write c) => B.ResultWriterJudge c
hPutJudgesWith h result status js =
    do cnt <- hPutJudgesCount h result (B.hPutJudge h) js $ judgeCount []
       B.hPutLines h $ judgeSummary status cnt
       return status

hPutJudgesCount :: forall c. (B.Write c) =>
    IO.Handle -> B.Result c -> (B.Judge c -> IO ()) -> [B.Judge c] -> JudgeCount -> IO JudgeCount
hPutJudgesCount h result writer = loop where
    loop (j : js) cnt  = loop js =<< put j cnt
    loop [] cnt@(c, _) = do M.when (c > 0) $ B.hPutEmptyLine h
                            total c
                            B.hPutEmptyLine h
                            return cnt

    put :: B.Judge c -> JudgeCount -> IO JudgeCount
    put judge (c, tab) = do putGutter c
                            writer judge
                            let c'  = c + 1
                                pat = B.judgePat judge
                            return (c', Map.alter inc pat tab)

    putGutter c   = M.when (mod5 c && c > 0) $
                      do M.when (mod25 c) $ progress c
                         B.hPutEmptyLine h

    mod25 n       = n `mod` measure == 0
    mod5  n       = n `mod` gutter  == 0
    gutter        = B.resultGutter  result
    measure       = B.resultMeasure result

    total    n    = IO.hPutStrLn h $ "*** " ++ count n
    progress n    = IO.hPutStrLn h $ "*** " ++ show n

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1


-- ----------------------  Counter

-- | Total and per-judgement counter
type JudgeCount = (Int, Map.Map B.JudgePat Int)

judgeCount :: [B.JudgePat] -> JudgeCount
judgeCount ps = (0, Map.fromList $ zip ps $ repeat 0)

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

