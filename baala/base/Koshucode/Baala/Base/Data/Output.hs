{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Output judgements.

module Koshucode.Baala.Base.Data.Output
  ( -- * Data type
    OutputResult,
    OutputChunks,
    OutputChunk (..),
    IOPoints,
  
    -- * Function
    putJudges,
    hPutJudges,
    putOutputResult,
  ) where

import qualified Control.Monad                     as M
import qualified Data.Map                          as Map
import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base.Abort        as B
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Judge   as B

hPutEmptyLine :: IO.Handle -> IO ()
hPutEmptyLine h = IO.hPutStrLn h ""

hPutLines :: IO.Handle -> [String] -> IO ()
hPutLines h = (IO.hPutStrLn h `mapM_`)


-- ----------------------  Output judges

-- total and per-judgement counter
type Counter = (Int, Map.Map String Int)

initialCounter :: Counter
initialCounter = (0, Map.empty)

-- | Print judges to `IO.stdout`.
putJudges :: (Ord c, B.Write c) => Int -> [B.Judge c] -> IO Int
putJudges = hPutJudges IO.stdout

hPutJudges :: (Ord c, B.Write c) => IO.Handle -> Int -> [B.Judge c] -> IO Int
hPutJudges h status js =
    do cnt <- judges h writer js initialCounter
       hPutLines h $ summary status cnt
       return status
    where
      writer = IO.hPutStrLn h . B.judgeShow B.shortEmpty

judges :: forall c. (Ord c, B.Write c) =>
    IO.Handle -> (B.Judge c -> IO ()) -> [B.Judge c] -> Counter -> IO Counter
judges h writer = loop where
    loop (j : js) cnt  = loop js =<< put j cnt
    loop [] cnt@(c, _) = do M.when (c > 0) $ hPutEmptyLine h
                            total c
                            hPutEmptyLine h
                            return cnt

    put :: B.Judge c -> Counter -> IO Counter
    put judge (c, tab) = do gutter c
                            writer judge
                            let c'  = c + 1
                                pat = B.judgePat judge
                            return (c', Map.alter inc pat tab)

    gutter c      = M.when (mod5 c && c > 0) $
                      do M.when (mod25 c) $ progress c
                         hPutEmptyLine h

    mod25 n       = (n `mod` 25 == 0)
    mod5  n       = (n `mod` 5  == 0)

    total    n    = IO.hPutStrLn h $ "*** " ++ showCount n
    progress n    = IO.hPutStrLn h $ "*** " ++ show n

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

-- hPutLines IO.stdout $ summary 0 (10, Map.fromList [("A", 3), ("B", 6), ("C", 1)])
summary :: Int -> Counter -> [String]
summary status (_, tab) = B.texts sumDoc where
    label | status == 0 =  "SUMMARY"
          | otherwise   =  "SUMMARY (VIOLATED)"

    sumDoc              =  B.CommentDoc [sumSec]
    sumSec              =  B.CommentSec label $ sumLines ++ [total]
    sumLines            =  map sumLine $ Map.assocs tab
    sumLine (p, n)      =  count n ++ " on " ++ p
    total               =  count (sumOf tab) ++ " in total"

    count n             =  B.padLeft 11 $ showCount n

sumOf :: Map.Map a Int -> Int
sumOf = Map.foldr (+) 0

showCount :: Int -> String
showCount 0 = "no judges"
showCount 1 = "1 judge "
showCount n = show n ++ " judges"


-- ----------------------  Output chunks

type OutputResult  = (B.CodeName, [OutputChunks], [OutputChunks])
type OutputChunks  = B.Short [OutputChunk]
type IOPoints      = ([B.CodeName], B.CodeName)

data OutputChunk
    = OutputJudge  [B.Judge String]
    | OutputNote   [String]
      deriving (Show, Eq, Ord)

-- | Print result of calculation, and return status.
putOutputResult :: IOPoints -> OutputResult -> IO Int
putOutputResult iop (B.CodeStdout, vio, jud) =
    hPutOutputResult IO.stdout iop vio jud
putOutputResult iop (B.CodeFile path, vio, jud) =
    do h <- IO.openFile path IO.WriteMode
       n <- hPutOutputResult h iop vio jud
       IO.hClose h
       return n
putOutputResult _ (p, _, _) = B.bug $ "putOutputResult " ++ show p

hPutOutputResult :: IO.Handle -> IOPoints -> [OutputChunks] -> [OutputChunks] -> IO Int
hPutOutputResult h iop vio jud
    | null vio2  = shortList h 0 iop jud
    | otherwise  = shortList h 1 iop vio2
    where
      vio2 :: [OutputChunks]
      vio2 = B.shortTrim $ B.map2 (filter $ existJudge) vio

      existJudge :: OutputChunk -> Bool
      existJudge (OutputNote _)    = False
      existJudge (OutputJudge [])  = False
      existJudge _                 = True

shortList :: IO.Handle -> Int -> IOPoints -> [OutputChunks] -> IO Int
shortList h status (inputs, output) sh =
    do let itext  = B.codeNameText `map` inputs
           otext  = B.codeNameText output
           comm   = B.CommentDoc [ B.CommentSec "INPUT"  itext
                                 , B.CommentSec "OUTPUT" [otext] ]

       IO.hSetEncoding h IO.utf8
       IO.hPutStrLn    h B.emacsModeComment
       IO.hPutStr      h $ unlines $ B.texts comm
       IO.hPutStrLn    h ""

       cnt <- M.foldM (short h) initialCounter sh
       hPutLines h $ summary status cnt
       return status

short :: IO.Handle -> Counter -> OutputChunks -> IO Counter
short h cnt (B.Short _ []  output) = chunks h output cnt
short h cnt (B.Short _ def output) =
    do hPutLines h $ "short" : map shortLine def
       hPutEmptyLine h
       chunks h output cnt
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ B.padRight width a ++
                         " "  ++ show b

      width :: Int
      width = maximum $ map (length . fst) def

chunks :: IO.Handle -> [OutputChunk] -> Counter -> IO Counter
chunks h = loop where
    writer = IO.hPutStrLn h . B.judgeLine

    loop [] cnt = return cnt
    loop (OutputJudge js : xs) (_, tab) = do cnt' <- judges h writer js (0, tab)
                                             loop xs cnt'
    loop (OutputNote [] : xs) cnt       = loop xs cnt
    loop (OutputNote ls : xs) cnt       = do hPutNote h $ unlines ls
                                             loop xs cnt

hPutNote :: IO.Handle -> String -> IO ()
hPutNote h s = do IO.hPutStrLn  h "=== note"
                  hPutEmptyLine h
                  IO.hPutStr    h s
                  hPutEmptyLine h
                  IO.hPutStrLn  h "=== rel"
                  hPutEmptyLine h
