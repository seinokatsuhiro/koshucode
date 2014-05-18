{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Output judgements.

module Koshucode.Baala.Base.Data.Output
(
  -- * Data type
  OutputResult,
  OutputChunks,
  OutputChunk (..),

  -- * Function
  putJudges,
  hPutJudges,
  hPutOutputResult,
) where

import qualified Control.Monad                     as M
import qualified Data.Map                          as Map
import qualified System.IO                         as IO
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
putJudges :: (Ord c, B.ShortDoc c) => Int -> [B.Judge c] -> IO Int
putJudges = hPutJudges IO.stdout

hPutJudges :: (Ord c, B.ShortDoc c) => IO.Handle -> Int -> [B.Judge c] -> IO Int
hPutJudges h status js =
    do cnt <- judges h [] js initialCounter
       hPutLines h $ summary status cnt
       return status

judges :: forall c. (Ord c, B.ShortDoc c) =>
    IO.Handle -> [B.ShortDef] -> [B.Judge c] -> Counter -> IO Counter
judges h sh = loop where
    loop (j : js) cnt  = loop js =<< put j cnt
    loop [] cnt@(c, _) = do M.when (c > 0) $ hPutEmptyLine h
                            total c
                            hPutEmptyLine h
                            return cnt

    put :: B.Judge c -> Counter -> IO Counter
    put judge@(B.Judge _ pat _) (c, tab) =
        do M.when (mod5 c && c > 0) $
            do M.when (mod25 c) $ progress c
               hPutEmptyLine h
           IO.hPutStrLn h $ show $ B.shortDoc sh judge
           let c' = c + 1
           return (c', Map.alter inc pat tab)

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

type OutputResult c = ([OutputChunks c], [OutputChunks c])
type OutputChunks c = B.Short [OutputChunk c]

data OutputChunk c
    = OutputJudge   [B.Judge c]
    | OutputComment [String]
      deriving (Show, Eq, Ord)

-- | Print result of calculation, and return status.
hPutOutputResult :: forall c. (Ord c, B.ShortDoc c) => IO.Handle -> OutputResult c -> IO Int
hPutOutputResult h (vio, jud)
    | null vio2  = shortList h 0 jud
    | otherwise  = shortList h 1 vio2
    where
      vio2 :: [OutputChunks c]
      vio2 = B.shortTrim $ B.shortMap (filter $ existJudge) vio

      existJudge :: OutputChunk c -> Bool
      existJudge (OutputComment _) = False
      existJudge (OutputJudge [])  = False
      existJudge _                 = True

shortList :: (Ord c, B.ShortDoc c) => IO.Handle -> Int -> [OutputChunks c] -> IO Int
shortList h status sh =
    do cnt <- M.foldM (short h) initialCounter sh
       hPutLines h $ summary status cnt
       return status

short :: (Ord c, B.ShortDoc c) => IO.Handle -> Counter -> OutputChunks c -> IO Counter
short h cnt (B.Short [] output) =
    do chunks h [] output cnt
short h cnt (B.Short sh output) =
    do hPutLines h $ "short" : map shortLine sh
       hPutEmptyLine h
       chunks h sh output cnt
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ a ++ " " ++ show b

chunks :: (Ord c, B.ShortDoc c) => IO.Handle -> [B.ShortDef]
       -> [OutputChunk c] -> Counter -> IO Counter
chunks h sh = loop where
    loop [] cnt = return cnt
    loop (OutputJudge js : xs) (_, tab) =
        do cnt' <- judges h sh js (0, tab)
           loop xs cnt'
    loop (OutputComment [] : xs) cnt = loop xs cnt
    loop (OutputComment ls : xs) cnt =
        do B.hPutCommentLines h ls
           hPutEmptyLine h
           loop xs cnt

