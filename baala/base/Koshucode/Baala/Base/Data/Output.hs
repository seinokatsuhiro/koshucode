{-# OPTIONS_GHC -Wall #-}

-- | Output judgements.

module Koshucode.Baala.Base.Data.Output
(
  -- * Data type
  OutputResult,
  OutputChunks,
  OutputChunk (..),

  -- * Function
  hPutOutputResult,
  hPutJudgesFlat,
  putJudges,
) where

import qualified Control.Monad                     as M
import qualified Data.Map                          as Map
import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Short   as B
import qualified Koshucode.Baala.Base.Data.Judge   as B

hPutEmptyLine :: IO.Handle -> IO ()
hPutEmptyLine h = IO.hPutStrLn h ""

hPutLines :: IO.Handle -> [String] -> IO ()
hPutLines h = (IO.hPutStrLn h `mapM_`)


-- ----------------------  Output judges

type Counter = (Int, Map.Map String Int)

initialCounter :: Counter
initialCounter = (0, Map.empty)

-- | Print judges to `IO.stdout`.
putJudges :: (Ord c, B.Pretty c) => Int -> [B.Judge c] -> IO Int
putJudges = hPutJudgesFlat IO.stdout

hPutJudgesFlat :: (Ord c, B.Pretty c) => IO.Handle -> Int -> [B.Judge c] -> IO Int
hPutJudgesFlat h status js =
    do cnt <- judges h initialCounter js
       hPutLines h $ summary status cnt
       return status

judges :: (Ord c, B.Pretty c) => IO.Handle -> Counter -> [B.Judge c] -> IO Counter
judges h = loop where
    loop cnt (j:js)     = do cnt' <- put j cnt
                             loop cnt' js
    loop cnt@(tt, _) [] = do M.when (tt `mod` 5 /= 0) $ hPutEmptyLine h
                             return cnt

    put judge@(B.Judge _ pat _) (tt, c) =
        do IO.hPutStrLn h $ show $ B.doc judge
           let tt' = tt + 1
           M.when (mod20 tt') $ counter tt'
           M.when (mod5  tt') $ hPutEmptyLine h
           return $ (tt', Map.alter inc pat c)

    mod20 n = (n `mod` 20 == 0)
    mod5  n = (n `mod` 5  == 0)

    counter n = IO.hPutStrLn h $ "*** " ++ show n ++ " judges"

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

-- hPutLines IO.stdout $ summary 0 (10, Map.fromList [("A", 3), ("B", 6), ("C", 1)])
summary :: Int -> Counter -> [String]
summary status (tt, c) = B.texts sumDoc where
    label | status == 0 =  "SUMMARY"
          | otherwise   =  "SUMMARY (VIOLATED)"

    sumDoc              =  B.CommentDoc [sumSec]
    sumSec              =  B.CommentSec label $ sumLines ++ [total]
    sumLines            =  map sumLine $ Map.assocs c
    sumLine (p, n)      =  count n ++ " on " ++ p
    total               =  count tt ++ " in total"

    count 0             =  comment $ "no judges"
    count 1             =  comment $ "1 judge "
    count n             =  comment $ show n ++ " judges"
    comment             =  B.padLeft 11


-- ----------------------  Output chunks

type OutputResult c = ([OutputChunks c], [OutputChunks c])
type OutputChunks c = B.Short [OutputChunk c]

data OutputChunk c
    = OutputJudge   [B.Judge c]
    | OutputComment [String]
      deriving (Show, Eq, Ord)

-- | Print result of calculation, and return status.
hPutOutputResult :: (Ord c, B.Pretty c) => IO.Handle -> OutputResult c -> IO Int
hPutOutputResult h ([], jud) = shortList h 0 jud
hPutOutputResult h (vio, _)  = shortList h 1 vio

shortList :: (Ord c, B.Pretty c) => IO.Handle -> Int -> [OutputChunks c] -> IO Int
shortList h status shorts =
    do cnt <- M.foldM (short h) initialCounter shorts
       hPutLines h $ summary status cnt
       return status

short :: (Ord c, B.Pretty c) => IO.Handle -> Counter -> OutputChunks c -> IO Counter
short h cnt (B.Short [] output) =
    do chunks h cnt output
short h cnt (B.Short shorts output) =
    do hPutLines h $ "short" : map shortLine shorts
       chunks h cnt output
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ a ++ " " ++ show b

chunks :: (Ord c, B.Pretty c) => IO.Handle -> Counter -> [OutputChunk c] -> IO Counter
chunks h = loop where
    loop cnt [] = return cnt
    loop cnt (OutputJudge js : xs) =
        do cnt' <- judges h cnt js
           loop cnt' xs
    loop cnt (OutputComment [] : xs) = loop cnt xs
    loop cnt (OutputComment ls : xs) =
        do B.putCommentLines ls
           putStrLn ""
           loop cnt xs

