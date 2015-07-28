{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Result of relational calculation.

module Koshucode.Baala.Base.Data.Result
  ( -- * Result
    Result (..), InputPoint (..),
    resultEmpty,

    -- * ResultChunk
    ResultShortChunks, ResultChunk (..),
  
    -- * Writer
    putResult, hPutResult,
    putJudges, putJudgesWith, hPutJudgesWith,
  ) where

import qualified Control.Monad                     as M
import qualified Data.Map                          as Map
import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base.Abort        as B
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Judge   as B
import qualified Koshucode.Baala.Base.Data.Rel     as B


-- ----------------------  Result

-- | Result of calculation.
data Result c = Result
    { resultPrintHead  :: Bool
    , resultPrintFoot  :: Bool
    , resultGutter     :: Int
    , resultMeasure    :: Int
    , resultInput      :: [InputPoint]
    , resultOutput     :: B.IOPoint
    , resultEcho       :: [[String]]
    , resultLicense    :: [[String]]
    , resultViolated   :: [ResultShortChunks c]
    , resultNormal     :: [ResultShortChunks c]
    , resultPattern    :: [B.JudgePat]
    } deriving (Show, Eq, Ord)

data InputPoint = InputPoint
    { inputPoint      :: B.IOPoint
    , inputPointAbout :: [B.TTree]
    } deriving (Show, Eq, Ord)

type ResultShortChunks c = B.Short [ResultChunk c]

-- | Chunk of judgements.
data ResultChunk c
    = ResultJudge  [B.Judge c]
    | ResultRel    B.JudgePat (B.Rel c)
    | ResultNote   [String]
      deriving (Show, Eq, Ord)

-- | Empty result.
resultEmpty :: Result c
resultEmpty =
    Result { resultPrintHead  = True
           , resultPrintFoot  = True
           , resultGutter     = 5
           , resultMeasure    = 25
           , resultInput      = []
           , resultOutput     = B.IOPointStdout
           , resultEcho       = []
           , resultLicense    = []
           , resultViolated   = []
           , resultNormal     = []
           , resultPattern    = [] }


-- ----------------------  Writer

-- | `IO.stdout` version of `hPutResult`.
putResult :: (Ord c, B.Write c) => Result c -> IO Int
putResult result =
    case resultOutput result of
      B.IOPointStdout ->
          hPutResult IO.stdout result
      B.IOPointFile _ path ->
          do h <- IO.openFile path IO.WriteMode
             n <- hPutResult h result
             IO.hClose h
             return n
      output -> B.bug $ "putResult " ++ show output

-- | Print result of calculation, and return status.
hPutResult :: forall c. (Ord c, B.Write c) => IO.Handle -> Result c -> IO Int
hPutResult h result
    -- | null vio   = do hPutRel h $ resultNormal result
    --                   return 0
    | null vio   = hPutAllChunks h 0 result $ resultNormal result
    | otherwise  = hPutAllChunks h 1 result vio
    where
      vio :: [ResultShortChunks c]
      vio = B.shortTrim $ B.map2 (filter hasJudge) $ resultViolated result

      hasJudge :: ResultChunk c -> Bool
      hasJudge (ResultJudge js)  = js /= []
      hasJudge _                 = False

hPutAllChunks :: (Ord c, B.Write c) => IO.Handle -> Int -> Result c -> [ResultShortChunks c] -> IO Int
hPutAllChunks h status result sh =
    do IO.hSetEncoding h IO.utf8
       -- head
       B.when (resultPrintHead result) $ hPutHead h result
       -- license
       hPutLicense h result
       -- echo
       hPutEcho h result
       -- body
       let cnt = initCounter $ resultPattern result
       cnt' <- M.foldM (hPutShortChunk h result) cnt sh
       -- foot
       B.when (resultPrintFoot result) $ hPutFoot h status cnt'
       return status

hPutRel :: (B.Write c) => IO.Handle -> [ResultShortChunks c] -> IO ()
hPutRel h sh = mapM_ put chunks where
    chunks = concatMap B.shortBody sh
    put (ResultRel _ r) = IO.hPutStrLn h $ B.renderHtml $ B.writeHtmlWith id r
    put _               = return ()

hPutLicense :: IO.Handle -> Result c -> IO ()
hPutLicense h Result { resultLicense = ls }
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

hPutEcho :: IO.Handle -> Result c -> IO ()
hPutEcho h result =
    do let echo = resultEcho result
       B.hPutLines h $ concat echo
       B.when (echo /= []) $ B.hPutEmptyLine h


-- ----------------------  Chunk

hPutShortChunk :: (Ord c, B.Write c) => IO.Handle -> Result c -> Counter -> ResultShortChunks c -> IO Counter
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

hPutChunks :: (Ord c, B.Write c) => IO.Handle -> Result c -> B.StringMap -> [ResultChunk c] -> Counter -> IO Counter
hPutChunks h result sh = loop where
    writer = IO.hPutStrLn h . B.writeDownJudge sh

    loop [] cnt                          = return cnt
    loop (ResultJudge js : xs) (_, tab)  = do cnt' <- hPutJudgesCount h result writer js (0, tab)
                                              loop xs cnt'
    loop (ResultNote [] : xs) cnt        = loop xs cnt
    loop (ResultNote ls : xs) cnt        = do hPutNote h ls
                                              loop xs cnt
    loop (ResultRel _ _ : xs) cnt        = loop xs cnt

hPutNote :: IO.Handle -> [String] -> IO ()
hPutNote h ls =
    do IO.hPutStrLn    h "=== note"
       B.hPutEmptyLine h
       B.hPutLines     h ls
       B.hPutEmptyLine h
       IO.hPutStrLn    h "=== rel"
       B.hPutEmptyLine h


-- ----------------------  Header and Footer

hPutHead :: IO.Handle -> Result c -> IO ()
hPutHead h result =
    do IO.hPutStrLn h B.emacsModeComment
       B.hPutLines  h $ B.texts comm
       B.hPutEmptyLine h
    where
      itext = (B.ioPointText . inputPoint) `map` resultInput result
      otext = B.ioPointText $ resultOutput result
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

putJudges :: (B.Write c, Ord c) => [B.Judge c] -> IO ()
putJudges js =
    do _ <- putJudgesWith 0 js
       return ()

-- | `IO.stdout` version of `hPutJudgesWith`.
putJudgesWith :: (Ord c, B.Write c) => Int -> [B.Judge c] -> IO Int
putJudgesWith = hPutJudgesWith IO.stdout resultEmpty

-- | Print list of judges.
hPutJudgesWith :: (Ord c, B.Write c) => IO.Handle -> Result c -> Int -> [B.Judge c] -> IO Int
hPutJudgesWith h result status js =
    do cnt <- hPutJudgesCount h result (B.hPutJudge h) js $ initCounter []
       B.hPutLines h $ summaryLines status cnt
       return status

hPutJudgesCount :: forall c. (Ord c, B.Write c) =>
    IO.Handle -> Result c -> (B.Judge c -> IO ()) -> [B.Judge c] -> Counter -> IO Counter
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
    gutter        = resultGutter  result
    measure       = resultMeasure result

    total    n    = IO.hPutStrLn h $ "*** " ++ judgeCount n
    progress n    = IO.hPutStrLn h $ "*** " ++ show n

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

