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
import qualified Koshucode.Baala.Data                as D
import qualified Koshucode.Baala.Core                as C


-- ----------------------  Writer

putJudges :: (Show c, B.Write c) => [D.Judge c] -> IO ()
putJudges js =
    do _ <- putJudgesWith (B.exitCode 0) js
       return ()

-- | `B.stdout` version of `hPutJudgesWith`.
putJudgesWith :: (Show c, B.Write c) => B.ExitCode -> [D.Judge c] -> IO B.ExitCode
putJudgesWith = hPutJudgesWith B.stdout B.def

-- | Print list of judges.
hPutJudgesWith :: (B.Write c) => C.ResultWriterJudge c
hPutJudgesWith h result status js =
    do cnt <- hPutJudgesCount h result (D.hPutJudge h) js $ judgeCount []
       B.hPutLines h $ judgeSummary status cnt
       return status

hPutJudgesCount :: forall c.
    IO.Handle -> C.Result c -> (D.Judge c -> IO ()) -> [D.Judge c] -> JudgeCount -> IO JudgeCount
hPutJudgesCount h result writer = loop where
    loop (j : js) cnt  = loop js =<< put j cnt
    loop [] cnt@(c, _) = do M.when (c > 0) $ B.hPutEmptyLine h
                            total c
                            B.hPutEmptyLine h
                            return cnt

    put :: D.Judge c -> JudgeCount -> IO JudgeCount
    put judge (c, tab) = do putGutter c
                            writer judge
                            let c'  = c + 1
                                cls = D.judgeClass judge
                            return (c', Map.alter inc cls tab)

    putGutter c   = M.when (mod5 c && c > 0) $
                      do M.when (mod25 c) $ progress c
                         B.hPutEmptyLine h

    mod25 n       = n `mod` measure == 0
    mod5  n       = n `mod` gutter  == 0
    gutter        = C.resultGutter  result
    measure       = C.resultMeasure result

    total    n    = IO.hPutStrLn h $ "*** " ++ count n
    progress n    = IO.hPutStrLn h $ "*** " ++ show n

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1


-- ----------------------  Counter

-- | Total and per-judgement counter
type JudgeCount = (Int, Map.Map D.JudgeClass Int)

judgeCount :: [D.JudgeClass] -> JudgeCount
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

