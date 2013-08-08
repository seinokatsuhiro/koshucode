{-# OPTIONS_GHC -Wall #-}

-- | Judgements: a symbolic representations of affirmed or denied statements.

module Koshucode.Baala.Base.Data.Judge
(
  -- * Datatype
  Judge (Judge),
  Relsign, Relarg,

  -- * Writer
  putJudges,
  hPutJudges,
  abcJudge,

  -- * Logical quality
  affirmJudge,
  denyJudge,
  isAffirmed,
  isDenied,
) where

import qualified Data.List as List
import qualified System.IO as IO
import Koshucode.Baala.Base.Prelude

{-| Judgement on type 'v'.
 
    Judgement (or judge for short) is divided into three parts:
    logical quality, sign of relation, and argument.
    'Bool' value of logical quality corresponds to
    affirmed or denied judge.
    'String' value of sign represents certain sentence pattern
    that gives intepretation of data.
    Sentence pattern has placeholders filled by
    ('String', 'v') values of argument. --} 

data Judge c = Judge Bool Relsign (Relarg c)
               deriving (Show, Eq, Ord)

-- | Sign of relation, or relation name.
type Relsign = String

-- | List of terms.
type Relarg c = [Named c]

-- Apply function to each values
instance Functor Judge where
    fmap f (Judge q s a) = Judge q s $ map g a
        where g (n, v) = (n, f v)

--  Pretty printing
instance (Ord c, Pretty c) => Pretty (Judge c) where
    doc (Judge q s a) = quality q <+> sign <+> arg a
        where
          -- Frege's judgement stroke, content line,
          -- and logical quality
          quality True  = text "|--"
          quality False = text "|-X"
          -- relsign
          sign | ':' `elem` s = docQuote $ text s
               | otherwise    = text s
          -- term name and term value
          arg ((n,v) : a2) = text " " <> text n <+> doc v <+> arg a2
          arg [] = empty



-- ----------------------  Logical quality

{-| Affirm judge, i.e., change logical quality to 'True'. -}
affirmJudge :: Map (Judge c)
affirmJudge (Judge _ s xs) = Judge True  s xs

{-| Deny judge, i.e., change logical quality to 'False'. -}
denyJudge :: Map (Judge c)
denyJudge (Judge _ s xs) = Judge False s xs

isAffirmed :: Judge t -> Bool
isAffirmed (Judge q _ _) = q

isDenied :: Judge t -> Bool
isDenied (Judge q _ _) = not q



-- ----------------------  Writer

putJudges :: (Ord c, Pretty c) => [Judge c] -> IO ()
putJudges = hPutJudges IO.stdout

hPutJudges :: (Ord c, Pretty c) => IO.Handle -> [Judge c] -> IO ()
hPutJudges h = IO.hPutStr h . unlines . showJudges

showJudges :: (Ord c, Pretty c) => [Judge c] -> [String]
showJudges = loop (1 :: Int) where
    count 0            =  "**  (no judges)"
    count 1            =  "**  (1 judge)"
    count n            =  "**  (" ++ show n ++ " judges)"

    grad   n           =  n `mod` 20  == 0
    gutter n           =  n `mod` 5   == 0

    loop _ []          =  count (0 :: Int) : [""]
    loop n [j]         =  s : count n : [""]
        where s        =  show $ doc j
    loop n (j : js)
        | grad   n     =  s : count n : "" : ss
        | gutter n     =  s : "" : ss
        | otherwise    =  s : ss
        where s        =  show $ doc j
              ss       =  loop (n + 1) js

{-| Sort terms in alphabetical order. -}
abcJudge :: (Ord c) => Judge c -> Judge c
abcJudge (Judge q s a) = Judge q s $ List.sort a

