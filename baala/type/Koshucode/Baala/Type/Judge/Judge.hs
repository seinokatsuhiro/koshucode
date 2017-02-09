{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Judgements: a symbolic representations of
--   affirmed or denied statements.

module Koshucode.Baala.Type.Judge.Judge
  (
    -- * Datatype
    Judge (..),
    judgeTermsMap,
    judgeAdd,

    -- * Logical quality
    JudgeOf,
    affirm, deny,
    affirmJudge, denyJudge,
    isAffirmative, isDenial, isViolative,
    AssertType (..),
    assertAs,
    assertSymbol,

    -- * Encode
    EncodeJudge,
    judgeBreak,
    judgeMix, judgeMix2, judgeMixTab, judgeMix2Tab,
    termNameToMix,
    termsToMix1, termsToMix2,
  ) where

import qualified Koshucode.Baala.Overture                    as O
import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Syntax                      as S
import qualified Koshucode.Baala.Type.Judge.JudgeClass       as T


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
    = JudgeAffirm      S.JudgeClass [S.Term c]             -- ^ @|-- P \/x 10 \/y 20@
    | JudgeDeny        S.JudgeClass [S.Term c]             -- ^ @|-x P \/x 10 \/y 20@
    | JudgeMultiDeny   S.JudgeClass [S.Term c]             -- ^ @|-xx P \/x 10 \/y 20@
    | JudgeChange      S.JudgeClass [S.Term c] [S.Term c]  -- ^ @|-c P \/x 10 +\/y 20@
    | JudgeMultiChange S.JudgeClass [S.Term c] [S.Term c]  -- ^ @|-cc P \/x 10 +\/y 20@
    | JudgeViolate     S.JudgeClass [S.Term c]             -- ^ @|-v P \/x 10 \/y 20@
      deriving (Show)

instance (Ord c) => Eq (Judge c) where
    j1 == j2  = compare j1 j2 == EQ
    j1 /= j2  = compare j1 j2 /= EQ

instance (Ord c) => Ord (Judge c) where
    compare j1 j2 =
        let j1'  = judgeNormalize j1
            j2'  = judgeNormalize j2
            p1   = judgeClass j1'
            p2   = judgeClass j2'
            xs1  = judgeTerms j1'
            xs2  = judgeTerms j2'
        in compare p1 p2 O.++ compare xs1 xs2

-- Apply function to each values
instance Functor Judge where
    fmap f j = judgeTermsMap (map g) j
        where g (n, c) = (n, f c)

-- | Map function to terms.
judgeTermsMap :: ([S.Term a] -> [S.Term b]) -> Judge a -> Judge b
judgeTermsMap f (JudgeAffirm      c xs)      = JudgeAffirm    c (f xs)
judgeTermsMap f (JudgeDeny        c xs)      = JudgeDeny      c (f xs)
judgeTermsMap f (JudgeMultiDeny   c xs)      = JudgeMultiDeny c (f xs)
judgeTermsMap f (JudgeChange      c xs xs')  = JudgeChange    c (f xs) (f xs')
judgeTermsMap f (JudgeMultiChange c xs xs')  = JudgeChange    c (f xs) (f xs')
judgeTermsMap f (JudgeViolate     c xs)      = JudgeViolate   c (f xs)

-- | Add a term into judgement.
judgeAdd :: S.Term c -> O.Map (Judge c)
judgeAdd x = judgeTermsMap (x :)

-- | Normalize judgement, i.e., sort terms in alphabetical order.
judgeNormalize :: (Ord c) => O.Map (Judge c)
judgeNormalize = judgeTermsMap O.sort


-- ----------------------  Logical quality

-- | Construct judgement from its class and terms.
type JudgeOf c = S.JudgeClass -> [S.Term c] -> Judge c

-- | Construct affirmative judgement.
--
--   >>> affirm "P" [("x",10), ("y",20)] :: Judge Int
--   JudgeAffirm "P" [("x",10), ("y",20)]
--
affirm :: JudgeOf c
affirm = JudgeAffirm

-- | Construct denial judgement.
deny :: JudgeOf c
deny = JudgeDeny

-- | Affirm judgement, i.e., change logical quality to affirmative.
affirmJudge :: O.Map (Judge c)
affirmJudge (JudgeDeny p xs) = JudgeAffirm p xs
affirmJudge j = j

-- | Deny judgement, i.e., change logical quality to denial.
--
--   >>> denyJudge $ affirm "P" [("x",10), ("y",20)] :: Judge Int
--   JudgeDeny "P" [("x",10), ("y",20)]
--
denyJudge :: O.Map (Judge c)
denyJudge (JudgeAffirm p xs) = JudgeDeny p xs
denyJudge j = j

-- | Test which judgement is affirmed.
isAffirmative :: O.Test (Judge c)
isAffirmative (JudgeAffirm _ _) = True
isAffirmative _                 = False

-- | Test which judgement is denied.
isDenial :: O.Test (Judge c)
isDenial (JudgeDeny _ _)        = True
isDenial (JudgeMultiDeny _ _)   = True
isDenial _                      = False

-- | Test which judgement is for violation.
isViolative :: O.Test (Judge c)
isViolative (JudgeViolate _ _)  = True
isViolative _                   = False

-- | Type of assertions.
data AssertType
    = AssertAffirm       -- ^ @|==@ /C/ @:@ /R/ generates affirmative judges.
    | AssertDeny         -- ^ @|=x@ /C/ @:@ /R/ generates denial judges.
    | AssertMultiDeny    -- ^ @|=xx@ /C/ @:@ /R/ generates multiple-denial judges.
    | AssertChange       -- ^ @|=c@ /C/ @:@ /R/ generates changement judges.
    | AssertMultiChange  -- ^ @|=cc@ /C/ @:@ /R/ generates multiple-changement judges.
    | AssertViolate      -- ^ @|=v@ /C/ @:@ /R/ generates violation judges.
      deriving (Show, Eq, Ord)

-- | Frege's stroke and various assertion lines.
--
--   >>> assertSymbol AssertAffirm
--   "|=="
--
assertSymbol :: AssertType -> String
assertSymbol AssertAffirm       = "|=="
assertSymbol AssertDeny         = "|=X"
assertSymbol AssertMultiDeny    = "|=XX"
assertSymbol AssertChange       = "|=C"
assertSymbol AssertMultiChange  = "|=CC"
assertSymbol AssertViolate      = "|=V"

-- | Create judgement corresponding to assertion type.
assertAs :: AssertType -> JudgeOf c
assertAs AssertAffirm        = JudgeAffirm
assertAs AssertDeny          = JudgeDeny
assertAs AssertMultiDeny     = JudgeMultiDeny
-- assertAs AssertChange        = JudgeChange
-- assertAs AssertMultiChange   = JudgeMultiChange
assertAs AssertViolate       = JudgeViolate


-- ----------------------  Class

instance T.GetClass (Judge c) where
    getClass = judgeClass

instance T.GetTermNames (Judge c) where
    getTermNames = map fst . judgeTerms

instance T.GetTerms Judge where
    getTerms = judgeTerms

-- | Return class of judgement.
judgeClass :: Judge c -> S.JudgeClass
judgeClass (JudgeAffirm      c _)      = c
judgeClass (JudgeDeny        c _)      = c
judgeClass (JudgeMultiDeny   c _)      = c
judgeClass (JudgeChange      c _ _)    = c
judgeClass (JudgeMultiChange c _ _)    = c
judgeClass (JudgeViolate     c _)      = c

-- | Return term list of judgement.
judgeTerms :: Judge c -> [S.Term c]
judgeTerms (JudgeAffirm      _ xs)     = xs
judgeTerms (JudgeDeny        _ xs)     = xs
judgeTerms (JudgeMultiDeny   _ xs)     = xs
judgeTerms (JudgeChange      _ xs _)   = xs
judgeTerms (JudgeMultiChange _ xs _)   = xs
judgeTerms (JudgeViolate     _ xs)     = xs


-- ----------------------  Encode

instance (B.MixEncode c) => B.MixEncode (Judge c) where
    mixTransEncode = judgeMix2

-- | Encode judgement with short sign converter.
type EncodeJudge t c = B.TransText t -> Judge c -> B.MixText

-- | Encode judgement with term separator.
judgeMix :: (O.Textual t, B.MixEncode c) => B.MixText -> EncodeJudge t c
judgeMix sep sh j =
    case j of
      JudgeAffirm      c xs    -> judge "|--"  c xs
      JudgeDeny        c xs    -> judge "|-X"  c xs
      JudgeMultiDeny   c xs    -> judge "|-XX" c xs
      JudgeChange      c xs _  -> judge "|-C"  c xs
      JudgeMultiChange c xs _  -> judge "|-CC" c xs
      JudgeViolate     c xs    -> judge "|-V"  c xs
    where
      judge sym c xs = B.mix sym `B.mixSep` B.mix c O.++ sep O.++ termsToMix sep sh xs

-- | Encode judgement with two-spaces term separator.
judgeMix2 :: (O.Textual t, B.MixEncode c) => EncodeJudge t c
judgeMix2 = judgeMix B.mix2

-- | Encode judgement with tab term separator.
judgeMixTab :: (O.Textual t, B.MixEncode c) => EncodeJudge t c
judgeMixTab = judgeMix B.mixTab

-- | Conditional judgement encoder.
--   If the first argument is true, 'judgeMix2' is used, otherwise 'judgeMixTab'.
judgeMix2Tab :: (O.Textual t, B.MixEncode c) => Bool -> EncodeJudge t c
judgeMix2Tab True   = judgeMix2
judgeMix2Tab False  = judgeMixTab

-- | Conventional line-break setting for judges:
--   4-spaces indent and 120-columns line.
judgeBreak :: B.LineBreak
judgeBreak = B.crlf4 120

-- | Encode term name.
--
--   >>> termNameToMix "foo"
--   MixText "/foo"
--
termNameToMix :: S.TermName -> B.MixText
termNameToMix n = B.mixString $ S.termNameString n

-- | Encode term list with one-space separator.
termsToMix1 :: (O.Textual t, B.MixEncode c) => B.TransText t -> [S.Term c] -> B.MixText
termsToMix1 = termsToMix B.mix1

-- | Encode term list with two-spaces separator.
termsToMix2 :: (O.Textual t, B.MixEncode c) => B.TransText t -> [S.Term c] -> B.MixText
termsToMix2 = termsToMix B.mix2

termsToMix :: (O.Textual t, B.MixEncode c) => B.MixText -> B.TransText t -> [S.Term c] -> B.MixText
termsToMix sep sh ts = B.mixJoin sep $ map term ts where
    term (n, c) = termNameToMix n `B.mixSep` B.mixTransEncode sh c

