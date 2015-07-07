{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Result of relational calculation.

module Koshucode.Baala.Base.Data.Result
  ( -- * Data type
    Result (..), InputPoint (..),
    ResultChunks, ResultChunk (..),
  
    -- * Function
    resultEmpty,
    putJudge, putJudges, hPutJudges,
    putResult, hPutResult,
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


-- ----------------------  Result judges

-- total and per-judgement counter
type Counter = (Int, Map.Map B.JudgePat Int)

initCounter :: [B.JudgePat] -> Counter
initCounter ps = (0, Map.fromList $ zip ps $ repeat 0)

putJudge :: (B.Write c) => B.Judge c -> IO ()
putJudge = putStrLn . B.writeDownJudge B.shortEmpty

-- | Print judges to `IO.stdout`.
putJudges :: (Ord c, B.Write c) => Int -> [B.Judge c] -> IO Int
putJudges = hPutJudges IO.stdout

hPutJudges :: (Ord c, B.Write c) => IO.Handle -> Int -> [B.Judge c] -> IO Int
hPutJudges h status js =
    do cnt <- judges h writer js $ initCounter []
       hPutLines h $ summary status cnt
       return status
    where
      writer = IO.hPutStrLn h . B.writeDownJudge B.shortEmpty

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


-- ----------------------  Result chunks

-- | Result of calculation.
data Result = 
    Result
    { resultInput    :: [InputPoint]
    , resultOutput   :: B.IOPoint
    , resultEcho     :: [[String]]
    , resultViolated :: [ResultChunks]
    , resultNormal   :: [ResultChunks]
    , resultPattern  :: [B.JudgePat]
    } deriving (Show, Eq, Ord)

data InputPoint = InputPoint
    { inputPoint      :: B.IOPoint
    , inputPointAbout :: [B.TTree]
    } deriving (Show, Eq, Ord)

type ResultChunks  = B.Short [ResultChunk]

data ResultChunk
    = ResultJudge  [B.Judge String]
    | ResultNote   [String]
      deriving (Show, Eq, Ord)

resultEmpty :: Result
resultEmpty =
    Result { resultInput    = []
           , resultOutput   = B.IOPointStdin
           , resultEcho     = []
           , resultViolated = []
           , resultNormal   = []
           , resultPattern  = [] }

-- | Print result of calculation, and return status.
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

hPutResult :: IO.Handle -> Result -> IO Int
hPutResult h ro
    | null vio   = shortList h 0 ro $ resultNormal ro
    | otherwise  = shortList h 1 ro vio
    where
      vio :: [ResultChunks]
      vio = B.shortTrim $ B.map2 (filter $ existJudge) $ resultViolated ro

      existJudge :: ResultChunk -> Bool
      existJudge (ResultNote _)    = False
      existJudge (ResultJudge [])  = False
      existJudge _                 = True

shortList :: IO.Handle -> Int -> Result -> [ResultChunks] -> IO Int
shortList h status ro sh =
    do let itext  = (B.ioPointText . inputPoint) `map` resultInput ro
           otext  = B.ioPointText $ resultOutput ro
           echo   = resultEcho ro
           comm   = B.CommentDoc [ B.CommentSec "INPUT"  itext
                                 , B.CommentSec "OUTPUT" [otext] ]

       IO.hSetEncoding h IO.utf8
       IO.hPutStrLn    h B.emacsModeComment
       IO.hPutStr      h $ unlines $ B.texts comm
       IO.hPutStrLn    h ""

       IO.hPutStr      h $ unlines $ concat echo
       B.when (echo /= []) $ IO.hPutStrLn h ""

       cnt <- M.foldM (short h) (initCounter $ resultPattern ro) sh
       hPutLines h $ summary status cnt
       return status

short :: IO.Handle -> Counter -> ResultChunks -> IO Counter
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

chunks :: IO.Handle -> [ResultChunk] -> Counter -> IO Counter
chunks h = loop where
    writer = IO.hPutStrLn h . B.judgeText

    loop [] cnt = return cnt
    loop (ResultJudge js : xs) (_, tab) = do cnt' <- judges h writer js (0, tab)
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
