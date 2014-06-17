{-# OPTIONS_GHC -Wall #-}

-- | Judgements: a symbolic representations of
--   affirmed or denied statements.

module Koshucode.Baala.Base.Data.Judge
(
  -- * Datatype
  Judge (..),
  JudgeOf,
  JudgePat,
  judgePat,
  judgeTerms,
  judgeTermsMap,
  judgeCons,
  abcJudge,

  -- * Logical quality
  affirm, deny,
  affirmJudge, denyJudge,
  isAffirmative, isDenial, isViolative,
) where

import qualified Koshucode.Baala.Base.Abort        as B
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

data Judge c
    = JudgeAffirm  JudgePat [B.Named c]
    | JudgeDeny    JudgePat [B.Named c]
    | JudgeViolate JudgePat [B.Named c]
    | JudgeAlter   JudgePat [B.Named c] [B.Named c] [B.Named c]
      deriving (Show)

instance (Ord c) => Eq (Judge c) where
    j1 == j2  =  compare j1 j2 == EQ
    j1 /= j2  =  compare j1 j2 /= EQ

instance (Ord c) => Ord (Judge c) where
    compare j1 j2 =
        let JudgeAffirm p1 xs1 = abcJudge j1
            JudgeAffirm p2 xs2 = abcJudge j2
        in compare p1 p2 `B.mappend` compare xs1 xs2

judgePat :: Judge c -> JudgePat
judgePat (JudgeAffirm  p _)      =  p
judgePat (JudgeDeny    p _)      =  p
judgePat (JudgeViolate p _)      =  p
judgePat (JudgeAlter   p _ _ _)  =  p

judgeTerms :: Judge c -> [B.Named c]
judgeTerms (JudgeAffirm  _ xs)      =  xs
judgeTerms (JudgeDeny    _ xs)      =  xs
judgeTerms (JudgeViolate _ xs)      =  xs
judgeTerms (JudgeAlter   _ xs _ _)  =  xs

type JudgeOf c = JudgePat -> [B.Named c] -> Judge c

-- | Name of judgement pattern.
type JudgePat = String

-- Apply function to each values
instance Functor Judge where
    fmap f j = judgeTermsMap (map g) j
        where g (n, v) = (n, f v)

instance (Ord c, B.Write c) => B.Write (Judge c) where
    write = judgeDoc 

judgeDoc :: (B.Write c) => B.StringMap -> Judge c -> B.Doc
judgeDoc shorts j =
    case j of
      -- Frege's judgement stroke, content line,
      JudgeAffirm  p xs -> B.doc "|--" B.<+> pat p B.<+> arg xs
      JudgeDeny    p xs -> B.doc "|-X" B.<+> pat p B.<+> arg xs
      JudgeViolate p xs -> B.doc "|-V" B.<+> pat p B.<+> arg xs
      JudgeAlter   p key den aff -> B.doc "|-Y" B.<+> pat p B.<+> arg key
                                    B.<+> B.doc "|-X" B.<+> arg den
                                    B.<+> B.doc "|--" B.<+> arg aff
    where
      -- pattern
      pat p | ':' `elem` p = B.docWrap "\"" "\"" p
            | otherwise    = B.doc p

      -- term name and term value
      arg ((n,c) : a2) = B.doc " " B.<> B.doc (B.showTermName n)
                         B.<+> B.write shorts c B.<+> arg a2
      arg [] = B.docEmpty

-- | Sort terms in alphabetical order.
abcJudge :: (Ord c) => B.Map (Judge c)
abcJudge = judgeTermsMap B.sort

judgeTermsMap :: ([B.Named a] -> [B.Named b]) -> Judge a -> Judge b
judgeTermsMap f (JudgeAffirm  p xs)       = JudgeAffirm  p (f xs)
judgeTermsMap f (JudgeDeny    p xs)       = JudgeDeny    p (f xs)
judgeTermsMap f (JudgeViolate p xs)       = JudgeViolate p (f xs)
judgeTermsMap f (JudgeAlter   p xs ys zs) = JudgeAlter   p (f xs) (f ys) (f zs)

judgeCons :: B.Named c -> B.Map (Judge c)
judgeCons x = judgeTermsMap (x :)


-- ----------------------  Logical quality

-- | Construct affirmative judgement.
affirm :: JudgeOf c
affirm = JudgeAffirm

-- | Construct denial judgement.
deny :: JudgeOf c
deny = JudgeDeny

-- | Affirm judgement, i.e., change logical quality to affirmative.
affirmJudge :: B.Map (Judge c)
affirmJudge (JudgeDeny p xs) = JudgeAffirm p xs
affirmJudge _ = B.bug "denyJudge"

-- | Deny judgement, i.e., change logical quality to denial.
denyJudge :: B.Map (Judge c)
denyJudge (JudgeAffirm p xs) = JudgeDeny p xs
denyJudge _ = B.bug "denyJudge"

-- | Test that judgement is affirmd.
isAffirmative :: Judge c -> Bool
isAffirmative (JudgeAffirm _ _) = True
isAffirmative _                 = False

-- | Test that judgement is denied.
isDenial :: Judge c -> Bool
isDenial (JudgeDeny _ _) = True
isDenial _               = False

isViolative :: Judge c -> Bool
isViolative (JudgeViolate _ _) = True
isViolative _                  = False

