{-# OPTIONS_GHC -Wall #-}

-- | Judgements: a symbolic representations of
--   affirmed or denied statements.

module Koshucode.Baala.Base.Data.Judge
  (
    -- * Datatype
    Judge (..), JudgePat,
    judgePat, judgeTerms,
    judgeCons,
    judgeTermsMap,
    sortJudgeTerms, 

    -- * Converter
    JudgeOf,
    SelectRel (..),
    judgesFromRel,

    -- * Writer
    writeDownJudge, writeDownTerms,
    textualjudge, judgeText,
    termText, termsText,
  
    -- * Logical quality
    affirm, deny,
    affirmJudge, denyJudge,
    isAffirmative, isDenial, isViolative,
  ) where

import qualified Koshucode.Baala.Base.Abort        as B
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Rel     as B
import qualified Koshucode.Baala.Base.Data.Head    as B



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
    = JudgeAffirm      JudgePat [B.Named c]
    | JudgeDeny        JudgePat [B.Named c]
    | JudgeMultiDeny   JudgePat [B.Named c]
    | JudgeChange      JudgePat [B.Named c] [B.Named c]
    | JudgeMultiChange JudgePat [B.Named c] [B.Named c]
    | JudgeViolate     JudgePat [B.Named c]
      deriving (Show)

instance (Ord c) => Eq (Judge c) where
    j1 == j2  =  compare j1 j2 == EQ
    j1 /= j2  =  compare j1 j2 /= EQ

instance (Ord c) => Ord (Judge c) where
    compare j1 j2 =
        let JudgeAffirm p1 xs1 = sortJudgeTerms j1
            JudgeAffirm p2 xs2 = sortJudgeTerms j2
        in compare p1 p2 `B.mappend` compare xs1 xs2

-- Apply function to each values
instance Functor Judge where
    fmap f j = judgeTermsMap (map g) j
        where g (n, v) = (n, f v)

-- | Return pattern of judgement.
judgePat :: Judge c -> JudgePat
judgePat (JudgeAffirm      p _)        = p
judgePat (JudgeDeny        p _)        = p
judgePat (JudgeMultiDeny   p _)        = p
judgePat (JudgeChange      p _ _)      = p
judgePat (JudgeMultiChange p _ _)      = p
judgePat (JudgeViolate     p _)        = p

-- | Return term list of judgement.
judgeTerms :: Judge c -> [B.Named c]
judgeTerms (JudgeAffirm      _ xs)     = xs
judgeTerms (JudgeDeny        _ xs)     = xs
judgeTerms (JudgeMultiDeny   _ xs)     = xs
judgeTerms (JudgeChange      _ xs _)   = xs
judgeTerms (JudgeMultiChange _ xs _)   = xs
judgeTerms (JudgeViolate     _ xs)     = xs

-- | Name of judgement pattern.
type JudgePat = String

-- | Sort terms in alphabetical order.
sortJudgeTerms :: (Ord c) => B.Map (Judge c)
sortJudgeTerms = judgeTermsMap B.sort

-- | Prepend a term into judgement.
judgeCons :: B.Named c -> B.Map (Judge c)
judgeCons x = judgeTermsMap (x :)

judgeTermsMap :: ([B.Named a] -> [B.Named b]) -> Judge a -> Judge b
judgeTermsMap f (JudgeAffirm      p xs)      = JudgeAffirm    p (f xs)
judgeTermsMap f (JudgeDeny        p xs)      = JudgeDeny      p (f xs)
judgeTermsMap f (JudgeMultiDeny   p xs)      = JudgeMultiDeny p (f xs)
judgeTermsMap f (JudgeChange      p xs ys)   = JudgeChange    p (f xs) (f ys)
judgeTermsMap f (JudgeMultiChange p xs ys)   = JudgeChange    p (f xs) (f ys)
judgeTermsMap f (JudgeViolate     p xs)      = JudgeViolate   p (f xs)


-- ----------------------  Converter

type JudgeOf c = JudgePat -> [B.Named c] -> Judge c

class SelectRel r where
    selectRel :: r c -> JudgePat -> [B.TermName] -> B.Rel c

-- | Convert relation to list of judges.
judgesFromRel :: JudgeOf c -> JudgePat -> B.Rel c -> [Judge c]
judgesFromRel judgeOf pat (B.Rel he bo) = map judge bo where
    judge = judgeOf pat . zip names
    names = B.headNames he


-- ----------------------  Writer

writeDownJudge :: (B.Write c) => B.StringMap -> Judge c -> String
writeDownJudge sh = judgeText . textualjudge sh

writeDownTerms :: (B.Write c) => B.StringMap -> [B.Named c] -> String
writeDownTerms sh = concatMap term where
    term (n, c) = termText n $ show $ B.write sh c

textualjudge :: (B.Write c) => B.StringMap -> Judge c -> Judge String
textualjudge sh = (text `fmap`) where
    text = show . B.write sh

judgeText :: Judge String -> String
judgeText jud =
    case jud of
      JudgeAffirm      p xs     -> line "|--"  p xs
      JudgeDeny        p xs     -> line "|-X"  p xs
      JudgeMultiDeny   p xs     -> line "|-XX" p xs
      JudgeChange      p xs _   -> line "|-C"  p xs
      JudgeMultiChange p xs _   -> line "|-CC" p xs
      JudgeViolate     p xs     -> line "|-V"  p xs
    where
      line j p xs  = j ++ " " ++ p ++ termsText xs

termText :: String -> String -> String
termText n c = "  /" ++ n ++ " " ++ c

termsText :: [B.Named String] -> String
termsText = concatMap term where
    term (n, c)  = termText n c


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
isDenial _                      = False

-- | Test which judgement is for violation.
isViolative :: Judge c -> Bool
isViolative (JudgeViolate _ _)  = True
isViolative _                   = False

