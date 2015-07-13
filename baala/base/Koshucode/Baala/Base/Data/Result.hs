{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Result of relational calculation.

module Koshucode.Baala.Base.Data.Result
  ( -- * Result
    Result (..), InputPoint (..),
    resultEmpty,

    -- * ResultChunk
    ResultChunks, ResultChunk (..),
  
    -- * Writer
    putResult, hPutResult,
    putJudges, hPutJudges,
  ) where

import qualified Control.Monad                     as M
import qualified Data.Map                          as Map
import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base.Abort        as B
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Judge   as B


-- ----------------------  Result

-- | Result of calculation.
data Result = Result
    { resultPrintHead :: Bool
    , resultInput     :: [InputPoint]
    , resultOutput    :: B.IOPoint
    , resultEcho      :: [[String]]
    , resultViolated  :: [ResultChunks]
    , resultNormal    :: [ResultChunks]
    , resultPattern   :: [B.JudgePat]
    } deriving (Show, Eq, Ord)

data InputPoint = InputPoint
    { inputPoint      :: B.IOPoint
    , inputPointAbout :: [B.TTree]
    } deriving (Show, Eq, Ord)

type ResultChunks  = B.Short [ResultChunk]

-- | Chunk of judgements.
data ResultChunk
    = ResultJudge  [B.Judge String]
    | ResultNote   [String]
      deriving (Show, Eq, Ord)

-- | Empty result.
resultEmpty :: Result
resultEmpty =
    Result { resultPrintHead = True
           , resultInput     = []
           , resultOutput    = B.IOPointStdout
           , resultEcho      = []
           , resultViolated  = []
           , resultNormal    = []
           , resultPattern   = [] }


-- ----------------------  Writer

hPutEmptyLine :: IO.Handle -> IO ()
hPutEmptyLine h = IO.hPutStrLn h ""

-- | `IO.stdout` version of `hPutResult`.
putResult :: Result -> IO Int
putResult ro =
    case resultOutput ro of
      B.IOPointStdout ->
          hPutResult IO.stdout ro
      B.IOPointFile _ path ->
          do h <- IO.openFile path IO.WriteMode
             n <- hPutResult h ro
             IO.hClose h
             return n
      output -> B.bug $ "putResult " ++ show output

-- | Print result of calculation, and return status.
hPutResult :: IO.Handle -> Result -> IO Int
hPutResult h result
    | null vio   = hPutAllChunks h 0 result $ resultNormal result
    | otherwise  = hPutAllChunks h 1 result vio
    where
      vio :: [ResultChunks]
      vio = B.shortTrim $ B.map2 (filter $ existJudge) $ resultViolated result

      existJudge :: ResultChunk -> Bool
      existJudge (ResultNote _)    = False
      existJudge (ResultJudge [])  = False
      existJudge _                 = True

hPutAllChunks :: IO.Handle -> Int -> Result -> [ResultChunks] -> IO Int
hPutAllChunks h status result sh =
    do IO.hSetEncoding h IO.utf8

       -- head
       B.when (resultPrintHead result) $ hPutHead h result

       -- echo
       let echo = resultEcho result
       B.hPutLines h $ concat echo
       B.when (echo /= []) $ IO.hPutStrLn h ""

       -- body
       cnt <- M.foldM (hPutShort h) (initCounter $ resultPattern result) sh
       B.hPutLines h $ summaryLines status cnt
       return status

hPutHead :: IO.Handle -> Result -> IO ()
hPutHead h result =
    do let itext  = (B.ioPointText . inputPoint) `map` resultInput result
           otext  = B.ioPointText $ resultOutput result
           comm   = B.CommentDoc [ B.CommentSec "INPUT"  itext
                                 , B.CommentSec "OUTPUT" [otext] ]

       IO.hPutStrLn h B.emacsModeComment
       B.hPutLines  h $ B.texts comm
       IO.hPutStrLn h ""

hPutShort :: IO.Handle -> Counter -> ResultChunks -> IO Counter
hPutShort h cnt (B.Short _ []  output) = hPutChunks h output cnt
hPutShort h cnt (B.Short _ def output) =
    do B.hPutLines h $ "short" : map shortLine def
       hPutEmptyLine h
       hPutChunks h output cnt
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ B.padRight width a ++
                         " "  ++ show b

      width :: Int
      width = maximum $ map (length . fst) def

hPutChunks :: IO.Handle -> [ResultChunk] -> Counter -> IO Counter
hPutChunks h = loop where
    writer = IO.hPutStrLn h . B.judgeText

    loop [] cnt = return cnt
    loop (ResultJudge js : xs) (_, tab) = do cnt' <- hPutJudgesCount h writer js (0, tab)
                                             loop xs cnt'
    loop (ResultNote [] : xs) cnt       = loop xs cnt
    loop (ResultNote ls : xs) cnt       = do hPutNote h $ unlines ls
                                             loop xs cnt

hPutNote :: IO.Handle -> String -> IO ()
hPutNote h s = do IO.hPutStrLn  h "=== note"
                  hPutEmptyLine h
                  IO.hPutStr    h s
                  hPutEmptyLine h
                  IO.hPutStrLn  h "=== rel"
                  hPutEmptyLine h


-- ----------------------  Output list of judges

-- total and per-judgement counter
type Counter = (Int, Map.Map B.JudgePat Int)

initCounter :: [B.JudgePat] -> Counter
initCounter ps = (0, Map.fromList $ zip ps $ repeat 0)

-- | `IO.stdout` version of `hPutJudges`.
putJudges :: (Ord c, B.Write c) => Int -> [B.Judge c] -> IO Int
putJudges = hPutJudges IO.stdout

-- | Print list of judges.
hPutJudges :: (Ord c, B.Write c) => IO.Handle -> Int -> [B.Judge c] -> IO Int
hPutJudges h status js =
    do cnt <- hPutJudgesCount h (B.hPutJudge h) js $ initCounter []
       B.hPutLines h $ summaryLines status cnt
       return status

hPutJudgesCount :: forall c. (Ord c, B.Write c) =>
    IO.Handle -> (B.Judge c -> IO ()) -> [B.Judge c] -> Counter -> IO Counter
hPutJudgesCount h writer = loop where
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

    mod25 n       = n `mod` 25 == 0
    mod5  n       = n `mod` 5  == 0

    total    n    = IO.hPutStrLn h $ "*** " ++ countText n
    progress n    = IO.hPutStrLn h $ "*** " ++ show n

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

-- B.putLines $ summaryLines 0 (10, Map.fromList [("A", 3), ("B", 6), ("C", 1)])
summaryLines :: Int -> Counter -> [String]
summaryLines status (_, tab) = B.texts sumDoc where
    label | status == 0 =  "SUMMARY"
          | otherwise   =  "SUMMARY (VIOLATED)"

    sumDoc              =  B.CommentDoc [sumSec]
    sumSec              =  B.CommentSec label $ sumLines ++ [total]
    sumLines            =  map sumLine $ Map.assocs tab
    sumLine (p, n)      =  count n ++ " on " ++ p
    total               =  count (sumOf tab) ++ " in total"

    count n             =  B.padLeft 11 $ countText n

sumOf :: Map.Map a Int -> Int
sumOf = Map.foldr (+) 0

countText :: Int -> String
countText 0 = "no judges"
countText 1 = "1 judge "
countText n = show n ++ " judges"
