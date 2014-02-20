{-# OPTIONS_GHC -Wall #-}

{-| Judgements: a symbolic representations of
    affirmed or denied statements. -}

module Koshucode.Baala.Base.Data.Judge
(
  -- * Datatype
  Judge (Judge),
  JudgePattern,
  abcJudge,

  -- * Logical quality
  affirm, deny,
  affirmJudge, denyJudge,
  isAffirmed, isDenied,

  -- * Writer
  ShortJudge,
  putJudges,
  hPutJudges,
  hPutJudgesFlat,
) where

import qualified Control.Monad as M
import qualified Data.Monoid   as Monoid
import qualified Data.List     as List
import qualified Data.Map      as Map
import qualified System.IO     as IO
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Data.Short   as B
import qualified Koshucode.Baala.Base.Data.Comment as B


-- ----------------------  Datatype

{-| Judgement on type 'c'.
 
    Judgement (or judge for short) is divided into three parts:
    logical quality, name of pattern, and argument.
    Boolean values 'True' or 'False' of logical quality
    corresponds to affirmed or denied judge.
    A name of judgement pattern represents
    certain sentence pattern that gives intepretation of data.
    Sentence pattern has placeholders filled by
    'B.Named' @c@ in argument. -} 

data Judge c = Judge Bool JudgePattern [B.Named c]
               deriving (Show)

instance (Ord c) => Eq (Judge c) where
    j1 == j2  =  compare j1 j2 == EQ
    j1 /= j2  =  compare j1 j2 /= EQ

instance (Ord c) => Ord (Judge c) where
    compare j1 j2 =
        let Judge q1 p1 xs1 = abcJudge j1
            Judge q2 p2 xs2 = abcJudge j2
        in compare p1 p2
               `Monoid.mappend` compare q1 q2
               `Monoid.mappend` compare xs1 xs2

{-| Name of judgement pattern. -}
type JudgePattern = String

-- Apply function to each values
instance Functor Judge where
    fmap f (Judge q p a) = Judge q p $ map g a
        where g (n, v) = (n, f v)

{-| >>> doc $ Judge True "P" [("/a", 10), ("/b", 20 :: Int)]
    |-- P  /a 10  /b 20 -}
instance (Ord c, B.Pretty c) => B.Pretty (Judge c) where
    doc (Judge q p a) = quality q B.<+> sign B.<+> arg a
        where
          -- Frege's judgement stroke, content line,
          -- and logical quality
          quality True  = B.doc "|--"
          quality False = B.doc "|-X"
          -- pattern
          sign | ':' `elem` p = B.docWrap "\"" "\"" p
               | otherwise    = B.doc p
          -- term name and term value
          arg ((n,v) : a2) = B.doc " " B.<> B.doc n
                             B.<+> B.doc v B.<+> arg a2
          arg [] = B.docEmpty

{-| Sort terms in alphabetical order. -}
abcJudge :: (Ord c) => B.Map (Judge c)
abcJudge (Judge q p a) = Judge q p $ List.sort a



-- ----------------------  Logical quality

{-| Construct affirmed judgement. -}
affirm :: JudgePattern -> [B.Named c] -> Judge c
affirm = Judge True

{-| Construct denied judgement. -}
deny :: JudgePattern -> [B.Named c] -> Judge c
deny = Judge False

{-| Affirm judgement, i.e., change logical quality to 'True'. -}
affirmJudge :: B.Map (Judge c)
affirmJudge (Judge _ p xs) = Judge True p xs

{-| Deny judgement, i.e., change logical quality to 'False'. -}
denyJudge   :: B.Map (Judge c)
denyJudge   (Judge _ p xs) = Judge False p xs

{-| Test that judgement is affirmd. -}
isAffirmed :: Judge c -> Bool
isAffirmed (Judge q _ _) = q

{-| Test that judgement is denied.

    >>> isDenied $ Judge True "A" []
    False  -}
isDenied   :: Judge c -> Bool
isDenied   (Judge q _ _) = not q



-- ----------------------  Writer

type ShortJudge c = B.Short [Judge c]

{-| Print judges to `IO.stdout`. -}
putJudges :: (Ord c, B.Pretty c) => Int -> [Judge c] -> IO Int
putJudges = hPutJudgesFlat IO.stdout

hPutJudges :: (Ord c, B.Pretty c) => IO.Handle -> ([ShortJudge c], [ShortJudge c]) -> IO Int
hPutJudges h ([], jud) = hPutJudgesStatus h 0 jud
hPutJudges h (vio, _)  = hPutJudgesStatus h 1 vio

{-| Print judges. -}
hPutJudgesStatus :: (Ord c, B.Pretty c) => IO.Handle -> Int -> [ShortJudge c] -> IO Int
hPutJudgesStatus h status sh =
    do (n, c) <- M.foldM (hPutJudgeShort h) (0, Map.empty) sh
       IO.hPutStr h $ unlines $ judgeSummary status (n, c)
       return status

hPutJudgeShort :: (Ord c, B.Pretty c) => IO.Handle -> Counter -> ShortJudge c -> IO Counter
hPutJudgeShort h nc (B.Short [] js) =
    do hPutJudgeBody h nc js
hPutJudgeShort h nc (B.Short shorts js) =
    do IO.hPutStrLn h "short"
       IO.hPutStrLn h $ unlines $ map shortLine shorts
       hPutJudgeBody h nc js
    where
      shortLine :: (String, String) -> String
      shortLine (a, b) = "  " ++ a ++ " " ++ show b

hPutJudgesFlat :: (Ord c, B.Pretty c) => IO.Handle -> Int -> [Judge c] -> IO Int
hPutJudgesFlat h status js =
    do (n, c) <- hPutJudgeBody h (0, Map.empty) js
       IO.hPutStr h $ unlines $ judgeSummary status (n, c)
       return status

type Counter = (Int, Map.Map String Int)

hPutJudgeBody :: (Ord c, B.Pretty c) => IO.Handle -> Counter -> [Judge c] -> IO Counter
hPutJudgeBody h = loop where
    loop nc (j:js)    = do nc' <- put j nc
                           loop nc' js
    loop nc@(n, _) [] = do M.when (n `mod` 5 /= 0) $ IO.hPutStrLn h ""
                           return nc

    put judge@(Judge _ pat _) (n, c) =
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

