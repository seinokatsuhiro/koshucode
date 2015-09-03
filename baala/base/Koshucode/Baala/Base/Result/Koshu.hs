{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Result.Koshu
  ( resultKoshu,
    putJudges, putJudgesWith, hPutJudgesWith,
  ) where

import qualified Control.Monad                       as M
import qualified Data.Map                            as Map
import qualified System.IO                           as IO
import qualified Koshucode.Baala.Base.Data           as B
import qualified Koshucode.Baala.Base.Prelude        as B
import qualified Koshucode.Baala.Base.Text           as B
import qualified Koshucode.Baala.Base.Token          as B
import qualified Koshucode.Baala.Base.Result.Result  as B

resultKoshu :: (B.Write c) => B.ResultWriter c
resultKoshu = B.ResultWriterChunk "koshu" hPutKoshu

hPutKoshu :: (B.Write c) => B.ResultWriterChunk c
hPutKoshu h result status sh =
    do -- head
       B.when (B.resultPrintHead result) $ hPutHead h result
       hPutLicense h result
       hPutEcho h result
       -- body
       let cnt = initCounter $ B.resultPattern result
       cnt' <- M.foldM (hPutShortChunk h result) cnt sh
       -- foot
       B.when (B.resultPrintFoot result) $ hPutFoot h status cnt'
       return status

hPutLicense :: IO.Handle -> B.Result c -> IO ()
hPutLicense h B.Result { B.resultLicense = ls }
    | null ls    = return ()
    | otherwise  = do mapM_ put ls
                      IO.hPutStrLn h "=== rel"
                      B.hPutEmptyLine h
    where
      put license =
          do IO.hPutStrLn h "=== license"
             B.hPutEmptyLine h
             B.hPutLines h license
             B.hPutEmptyLine h

hPutEcho :: IO.Handle -> B.Result c -> IO ()
hPutEcho h result =
    do let echo = B.resultEcho result
       B.hPutLines h $ concat echo
       B.when (echo /= []) $ B.hPutEmptyLine h


-- ----------------------  Chunk

hPutShortChunk :: (B.Write c) => IO.Handle -> B.Result c -> Counter -> B.ShortResultChunks c -> IO Counter
hPutShortChunk h result cnt (B.Short _ def output) =
    do hPutShort h def
       hPutChunks h result (B.shortText def) output cnt

hPutShort :: IO.Handle -> [B.ShortDef] -> IO ()
hPutShort _ [] = return ()
hPutShort h def =
    do B.hPutLines h $ "short" : map shortLine def
       B.hPutEmptyLine h
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ B.padRight width a ++
                         " "  ++ show b
      width :: Int
      width = maximum $ map (length . fst) def

hPutChunks :: (B.Write c) => IO.Handle -> B.Result c -> B.StringMap -> [B.ResultChunk c] -> Counter -> IO Counter
hPutChunks h result sh = loop where
    writer = IO.hPutStrLn h . B.writeDownJudge sh

    loop [] cnt                            = return cnt
    loop (B.ResultJudge js : xs) (_, tab)  = do cnt' <- hPutJudgesCount h result writer js (0, tab)
                                                loop xs cnt'
    loop (B.ResultNote [] : xs) cnt        = loop xs cnt
    loop (B.ResultNote ls : xs) cnt        = do hPutNote h ls
                                                loop xs cnt
    loop (B.ResultRel _ _ : xs) cnt        = loop xs cnt

hPutNote :: IO.Handle -> [String] -> IO ()
hPutNote h ls =
    do IO.hPutStrLn    h "=== note"
       B.hPutEmptyLine h
       B.hPutLines     h ls
       B.hPutEmptyLine h
       IO.hPutStrLn    h "=== rel"
       B.hPutEmptyLine h


-- ----------------------  Header and Footer

hPutHead :: IO.Handle -> B.Result c -> IO ()
hPutHead h result =
    do IO.hPutStrLn h B.emacsModeComment
       B.hPutLines  h $ B.texts comm
       B.hPutEmptyLine h
    where
      itext = (B.ioPointText . B.inputPoint) `map` B.resultInput result
      otext = B.ioPointText $ B.resultOutput result
      comm  = B.CommentDoc [ B.CommentSec "INPUT"  itext
                           , B.CommentSec "OUTPUT" [otext] ]

hPutFoot :: IO.Handle -> Int -> Counter -> IO ()
hPutFoot h status cnt = B.hPutLines h $ summaryLines status cnt

-- B.putLines $ summaryLines 0 (10, Map.fromList [("A", 3), ("B", 6), ("C", 1)])
summaryLines :: Int -> Counter -> [String]
summaryLines status (_, tab) = B.texts sumDoc where
    label | status == 0 = "SUMMARY"
          | otherwise   = "SUMMARY (VIOLATED)"

    sumDoc              = B.CommentDoc [sumSec]
    sumSec              = B.CommentSec label $ sumLines ++ [total]
    sumLines            = map sumLine $ Map.assocs tab
    sumLine (p, n)      = count n ++ " on " ++ p
    total               = count (sumOf tab) ++ " in total"

    count n             = B.padLeft 11 $ judgeCount n

    sumOf :: Map.Map a Int -> Int
    sumOf = Map.foldr (+) 0

judgeCount :: Int -> String
judgeCount 0  = "no judges"
judgeCount 1  = "1 judge "
judgeCount n  = show n ++ " judges"


-- ----------------------  List of judges

-- total and per-judgement counter
type Counter = (Int, Map.Map B.JudgePat Int)

initCounter :: [B.JudgePat] -> Counter
initCounter ps = (0, Map.fromList $ zip ps $ repeat 0)

putJudges :: (Show c, B.Write c) => [B.Judge c] -> IO ()
putJudges js =
    do _ <- putJudgesWith 0 js
       return ()

-- | `B.stdout` version of `hPutJudgesWith`.
putJudgesWith :: (Show c, B.Write c) => Int -> [B.Judge c] -> IO Int
putJudgesWith = hPutJudgesWith B.stdout B.resultEmpty

-- | Print list of judges.
hPutJudgesWith :: (B.Write c) => IO.Handle -> B.Result c -> Int -> [B.Judge c] -> IO Int
hPutJudgesWith h result status js =
    do cnt <- hPutJudgesCount h result (B.hPutJudge h) js $ initCounter []
       B.hPutLines h $ summaryLines status cnt
       return status

hPutJudgesCount :: forall c. (B.Write c) =>
    IO.Handle -> B.Result c -> (B.Judge c -> IO ()) -> [B.Judge c] -> Counter -> IO Counter
hPutJudgesCount h result writer = loop where
    loop (j : js) cnt  = loop js =<< put j cnt
    loop [] cnt@(c, _) = do M.when (c > 0) $ B.hPutEmptyLine h
                            total c
                            B.hPutEmptyLine h
                            return cnt

    put :: B.Judge c -> Counter -> IO Counter
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

    total    n    = IO.hPutStrLn h $ "*** " ++ judgeCount n
    progress n    = IO.hPutStrLn h $ "*** " ++ show n

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

