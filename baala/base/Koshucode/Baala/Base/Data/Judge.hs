{-# OPTIONS_GHC -Wall #-}

-- | Judgements: a symbolic representations of
--   affirmed or denied statements.

module Koshucode.Baala.Base.Data.Judge
(
  -- * Datatype
  Judge (Judge),
  JudgePat,
  abcJudge,

  -- * Logical quality
  affirm, deny,
  affirmJudge, denyJudge,
  isAffirmed, isDenied,
) where

import qualified Data.List                         as List
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Data.Term    as B


-- ----------------------  Datatype

-- | Judgement on type 'c'.
--
--   Judgement (or judge for short) is divided into three parts:
--   logical quality, name of pattern, and argument.
--   Boolean values 'True' or 'False' of logical quality
--   corresponds to affirmed or denied judge.
--   A name of judgement pattern represents
--   certain sentence pattern that gives intepretation of data.
--   Sentence pattern has placeholders filled by
--   'B.Named' @c@ in argument.

data Judge c = Judge Bool JudgePat [B.Named c]
               deriving (Show)

instance (Ord c) => Eq (Judge c) where
    j1 == j2  =  compare j1 j2 == EQ
    j1 /= j2  =  compare j1 j2 /= EQ

instance (Ord c) => Ord (Judge c) where
    compare j1 j2 =
        let Judge q1 p1 xs1 = abcJudge j1
            Judge q2 p2 xs2 = abcJudge j2
        in compare p1 p2
               `B.mappend` compare q1 q2
               `B.mappend` compare xs1 xs2

-- | Name of judgement pattern.
type JudgePat = String

-- Apply function to each values
instance Functor Judge where
    fmap f (Judge q p a) = Judge q p $ map g a
        where g (n, v) = (n, f v)

instance (Ord c, B.ShortDoc c) => B.ShortDoc (Judge c) where
    shortDoc = judgeDoc 

judgeDoc :: (B.ShortDoc c) => B.StringMap -> Judge c -> B.Doc
judgeDoc shorts (Judge q p a) = quality q B.<+> sign B.<+> arg a where

    -- Frege's judgement stroke, content line,
    -- and logical quality
    quality True  = B.doc "|--"
    quality False = B.doc "|-X"

    -- pattern
    sign | ':' `elem` p = B.docWrap "\"" "\"" p
         | otherwise    = B.doc p

    -- term name and term value
    arg ((n,c) : a2) = B.doc " " B.<> B.doc (B.showTermName n)
                       B.<+> B.shortDoc shorts c B.<+> arg a2
    arg [] = B.docEmpty

-- | Sort terms in alphabetical order.
abcJudge :: (Ord c) => B.Map (Judge c)
abcJudge (Judge q p a) = Judge q p $ List.sort a



-- ----------------------  Logical quality

-- | Construct affirmed judgement.
affirm :: JudgePat -> [B.Named c] -> Judge c
affirm = Judge True

-- | Construct denied judgement.
deny :: JudgePat -> [B.Named c] -> Judge c
deny = Judge False

-- | Affirm judgement, i.e., change logical quality to 'True'.
affirmJudge :: B.Map (Judge c)
affirmJudge (Judge _ p xs) = Judge True p xs

-- | Deny judgement, i.e., change logical quality to 'False'.
denyJudge   :: B.Map (Judge c)
denyJudge   (Judge _ p xs) = Judge False p xs

-- | Test that judgement is affirmd.
isAffirmed :: Judge c -> Bool
isAffirmed (Judge q _ _) = q

-- | Test that judgement is denied.
--
--   >>> isDenied $ Judge True "A" []
--   False
isDenied   :: Judge c -> Bool
isDenied   (Judge q _ _) = not q

