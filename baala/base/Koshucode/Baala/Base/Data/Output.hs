{-# OPTIONS_GHC -Wall #-}

-- | Judgements: a symbolic representations of
--   affirmed or denied statements.

module Koshucode.Baala.Base.Data.Output
(
  ShortJudge,
  OutputChunk (..),
  putJudges,
  hPutJudges,
  hPutJudgesFlat,
) where

import qualified Control.Monad                     as M
import qualified Data.Map                          as Map
import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Short   as B
import qualified Koshucode.Baala.Base.Data.Judge   as B


type ShortJudge c = B.Short [OutputChunk c]

data OutputChunk c
    = OutputJudge   [B.Judge c]
    | OutputComment [String]
      deriving (Show, Eq, Ord)

-- | Print judges to `IO.stdout`.
putJudges :: (Ord c, B.Pretty c) => Int -> [B.Judge c] -> IO Int
putJudges = hPutJudgesFlat IO.stdout

hPutJudges :: (Ord c, B.Pretty c) => IO.Handle -> ([ShortJudge c], [ShortJudge c]) -> IO Int
hPutJudges h ([], jud) = hPutJudgesStatus h 0 jud
hPutJudges h (vio, _)  = hPutJudgesStatus h 1 vio

-- | Print judges.
hPutJudgesStatus :: (Ord c, B.Pretty c) => IO.Handle -> Int -> [ShortJudge c] -> IO Int
hPutJudgesStatus h status sh =
    do (n, c) <- M.foldM (hPutJudgeShort h) (0, Map.empty) sh
       IO.hPutStr h $ unlines $ judgeSummary status (n, c)
       return status

hPutJudgeShort :: (Ord c, B.Pretty c) => IO.Handle -> Counter -> ShortJudge c -> IO Counter
hPutJudgeShort h nc (B.Short [] output) =
    do hPutOutput h nc output
hPutJudgeShort h nc (B.Short shorts output) =
    do IO.hPutStrLn h "short"
       IO.hPutStrLn h $ unlines $ map shortLine shorts
       hPutOutput h nc output
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ a ++ " " ++ show b

hPutOutput :: (Ord c, B.Pretty c) => IO.Handle -> Counter -> [OutputChunk c] -> IO Counter
hPutOutput _ nc [] = return nc
hPutOutput h nc (OutputJudge js : xs) =
    do nc' <- hPutJudgeBody h nc js
       hPutOutput h nc' xs
hPutOutput h nc (OutputComment [] : xs) =
       hPutOutput h nc xs
hPutOutput h nc (OutputComment ls : xs) =
    do B.putCommentLines ls
       putStrLn ""
       hPutOutput h nc xs

hPutJudgesFlat :: (Ord c, B.Pretty c) => IO.Handle -> Int -> [B.Judge c] -> IO Int
hPutJudgesFlat h status js =
    do (n, c) <- hPutJudgeBody h (0, Map.empty) js
       IO.hPutStr h $ unlines $ judgeSummary status (n, c)
       return status

type Counter = (Int, Map.Map String Int)

hPutJudgeBody :: (Ord c, B.Pretty c) => IO.Handle -> Counter -> [B.Judge c] -> IO Counter
hPutJudgeBody h = loop where
    loop nc (j:js)    = do nc' <- put j nc
                           loop nc' js
    loop nc@(n, _) [] = do M.when (n `mod` 5 /= 0) $ IO.hPutStrLn h ""
                           return nc

    put judge@(B.Judge _ pat _) (n, c) =
        do IO.hPutStrLn h $ show $ B.doc judge
           let n' = n + 1
           M.when (n' `mod` 20 == 0) $ counter n'
           M.when (n' `mod`  5 == 0) $ gutter
           return $ (n', Map.alter inc pat c)

    counter n = IO.hPutStrLn h $ "*** " ++ show n ++ " judges"
    gutter    = IO.hPutStrLn h ""

    inc (Nothing) = Just 1
    inc (Just n)  = Just $ n + 1

-- >>> putStr . unlines $ judgeSummary 10 [("A", 3), ("B", 6), ("C", 1)]
judgeSummary :: Int -> Counter -> [String]
judgeSummary status (tt, c) = B.texts summaryDoc where
    label | status == 0 = "SUMMARY"
          | otherwise   = "SUMMARY (VIOLATED)"

    summaryDoc          =  B.CommentDoc [summary]
    summary             =  B.CommentSec label $ sumLines ++ [total tt]
    sumLines            =  map sumLine $ Map.assocs c
    sumLine (p, n)      =  count n ++ " on " ++ p
    total n             =  count n ++ " in total"

    count 0             =  comment $ "no judges"
    count 1             =  comment $ "1 judge "
    count n             =  comment $ show n ++ " judges"
    comment j           =  B.padLeft 11 j

