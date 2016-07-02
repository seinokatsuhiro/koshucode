{-# OPTIONS_GHC -Wall #-}

-- | Judgements: a symbolic representations of
--   affirmed or denied statements.

module Koshucode.Baala.Data.Type.Judge
  (
    -- * Datatype
    Judge (..), JudgeClass,
    judgeClass, judgeTerms,
    judgeTermsMap,
    judgeCons,
    sortJudgeTerms, 

    -- * Logical quality
    JudgeOf,
    affirm, deny,
    affirmJudge, denyJudge,
    isAffirmative, isDenial, isViolative,

    -- * Encode
    judgeBreak,
    termNameToMix,
    termsToMix1, termsToMix2,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S


-- ----------------------  Datatype

-- | Judgement on type 'c'.
--
--   Judgement (or judge for short) is divided into three parts:
--   logical quality, name of class, and argument.
--   Boolean values 'True' or 'False' of logical quality
--   corresponds to affirmed or denied judge.
--   A name of judgement class represents
--   certain sentence class that gives intepretation of data.
--   Sentence class has placeholders filled by
--   'B.Named' @c@ in argument.

data Judge c
    = JudgeAffirm      JudgeClass [S.Term c]             -- ^ @|-- P \/x 10 \/y 20@
    | JudgeDeny        JudgeClass [S.Term c]             -- ^ @|-x P \/x 10 \/y 20@
    | JudgeMultiDeny   JudgeClass [S.Term c]             -- ^ @|-xx P \/x 10 \/y 20@
    | JudgeChange      JudgeClass [S.Term c] [S.Term c]  -- ^ @|-c P \/x 10 +\/y 20@
    | JudgeMultiChange JudgeClass [S.Term c] [S.Term c]  -- ^ @|-cc P \/x 10 +\/y 20@
    | JudgeViolate     JudgeClass [S.Term c]             -- ^ @|-v P \/x 10 \/y 20@
      deriving (Show)

instance (Ord c) => Eq (Judge c) where
    j1 == j2  =  compare j1 j2 == EQ
    j1 /= j2  =  compare j1 j2 /= EQ

instance (Ord c) => Ord (Judge c) where
    compare j1 j2 =
        let j1'  = sortJudgeTerms j1
            j2'  = sortJudgeTerms j2
            p1   = judgeClass j1'
            p2   = judgeClass j2'
            xs1  = judgeTerms j1'
            xs2  = judgeTerms j2'
        in compare p1 p2 B.<> compare xs1 xs2

-- Apply function to each values
instance Functor Judge where
    fmap f j = judgeTermsMap (map g) j
        where g (n, v) = (n, f v)

instance (B.MixShortEncode c) => B.MixShortEncode (Judge c) where
    mixShortEncode sh j =
        case j of
          JudgeAffirm      c xs    -> judge "|--"  c xs
          JudgeDeny        c xs    -> judge "|-X"  c xs
          JudgeMultiDeny   c xs    -> judge "|-XX" c xs
          JudgeChange      c xs _  -> judge "|-C"  c xs
          JudgeMultiChange c xs _  -> judge "|-CC" c xs
          JudgeViolate     c xs    -> judge "|-V"  c xs
        where
          judge sym c xs = B.mix sym `B.mixSep` B.mix c `B.mixSep2` termsToMix2 sh xs

-- | Name of judgement class, in other words, name of propositional function.
type JudgeClass = String

-- | Return class of judgement.
judgeClass :: Judge c -> JudgeClass
judgeClass (JudgeAffirm      c _)        = c
judgeClass (JudgeDeny        c _)        = c
judgeClass (JudgeMultiDeny   c _)        = c
judgeClass (JudgeChange      c _ _)      = c
judgeClass (JudgeMultiChange c _ _)      = c
judgeClass (JudgeViolate     c _)        = c

-- | Return term list of judgement.
judgeTerms :: Judge c -> [S.Term c]
judgeTerms (JudgeAffirm      _ xs)     = xs
judgeTerms (JudgeDeny        _ xs)     = xs
judgeTerms (JudgeMultiDeny   _ xs)     = xs
judgeTerms (JudgeChange      _ xs _)   = xs
judgeTerms (JudgeMultiChange _ xs _)   = xs
judgeTerms (JudgeViolate     _ xs)     = xs

judgeTermsMap :: ([S.Term a] -> [S.Term b]) -> Judge a -> Judge b
judgeTermsMap f (JudgeAffirm      c xs)      = JudgeAffirm    c (f xs)
judgeTermsMap f (JudgeDeny        c xs)      = JudgeDeny      c (f xs)
judgeTermsMap f (JudgeMultiDeny   c xs)      = JudgeMultiDeny c (f xs)
judgeTermsMap f (JudgeChange      c xs xs')  = JudgeChange    c (f xs) (f xs')
judgeTermsMap f (JudgeMultiChange c xs xs')  = JudgeChange    c (f xs) (f xs')
judgeTermsMap f (JudgeViolate     c xs)      = JudgeViolate   c (f xs)

-- | Prepend a term into judgement.
judgeCons :: S.Term c -> B.Map (Judge c)
judgeCons x = judgeTermsMap (x :)

-- | Sort terms in alphabetical order.
sortJudgeTerms :: (Ord c) => B.Map (Judge c)
sortJudgeTerms = judgeTermsMap B.sort


-- ----------------------  Logical quality

-- | Construct judgement from its class and terms.
type JudgeOf c = JudgeClass -> [S.Term c] -> Judge c

-- | Construct affirmative judgement.
affirm :: JudgeOf c
affirm = JudgeAffirm

-- | Construct denial judgement.
deny :: JudgeOf c
deny = JudgeDeny

-- | Affirm judgement, i.e., change logical quality to affirmative.
affirmJudge :: B.Map (Judge c)
affirmJudge (JudgeDeny p xs) = JudgeAffirm p xs
affirmJudge _ = B.bug "affirmJudge"

-- | Deny judgement, i.e., change logical quality to denial.
denyJudge :: B.Map (Judge c)
denyJudge (JudgeAffirm p xs) = JudgeDeny p xs
denyJudge _ = B.bug "denyJudge"

-- | Test which judgement is affirmed.
isAffirmative :: Judge c -> Bool
isAffirmative (JudgeAffirm _ _) = True
isAffirmative _                 = False

-- | Test which judgement is denied.
isDenial :: Judge c -> Bool
isDenial (JudgeDeny _ _)        = True
isDenial (JudgeMultiDeny _ _)   = True
isDenial _                      = False

-- | Test which judgement is for violation.
isViolative :: Judge c -> Bool
isViolative (JudgeViolate _ _)  = True
isViolative _                   = False


-- ----------------------  Encode

-- | Conventional line-break setting for judges:
--   4-spaces indent and 120-columns line.
judgeBreak :: B.LineBreak
judgeBreak = B.crlf4 120

-- | Encode term name.
termNameToMix :: S.TermName -> B.MixText
termNameToMix n = B.mixString ('/' : n)

-- | Encode term list with one-space separator.
termsToMix1 :: (B.MixShortEncode c) => B.Shorten -> [S.Term c] -> B.MixText
termsToMix1 = termsToMix B.mix1

-- | Encode term list with two-spaces separator.
termsToMix2 :: (B.MixShortEncode c) => B.Shorten -> [S.Term c] -> B.MixText
termsToMix2 = termsToMix B.mix2

termsToMix :: (B.MixShortEncode c) => B.MixText -> B.Shorten -> [S.Term c] -> B.MixText
termsToMix sep sh ts = B.mixJoin sep $ map term ts where
    term (n, c) = termNameToMix n `B.mixSep` B.mixShortEncode sh c

