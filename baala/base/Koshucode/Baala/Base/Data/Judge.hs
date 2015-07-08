{-# OPTIONS_GHC -Wall #-}

-- | Judgements: a symbolic representations of
--   affirmed or denied statements.

module Koshucode.Baala.Base.Data.Judge
  (
    -- * Datatype
    Judge (..), JudgePat,
    judgePat, judgeTerms,
    judgeTermsMap,
    judgeCons,
    sortJudgeTerms, 

    -- * Logical quality
    JudgeOf,
    affirm, deny,
    affirmJudge, denyJudge,
    isAffirmative, isDenial, isViolative,

    -- * Writer
    writeDownJudge, writeDownTerms,
    textualjudge, judgeText,
    termText, termsText,
    putJudge, hPutJudge,
  ) where

import qualified System.IO                         as IO
import qualified Koshucode.Baala.Base.Abort        as B
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B


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
    = JudgeAffirm      JudgePat [B.Named c]  -- ^ @|-- P \/x 10 \/y 20@
    | JudgeDeny        JudgePat [B.Named c]  -- ^ @|-x P \/x 10 \/y 20@
    | JudgeMultiDeny   JudgePat [B.Named c]  -- ^ @|-xx P \/x 10 \/y 20@
    | JudgeChange      JudgePat [B.Named c] [B.Named c]  -- ^ @|-c P \/x 10 +\/y 20@
    | JudgeMultiChange JudgePat [B.Named c] [B.Named c]  -- ^ @|-cc P \/x 10 +\/y 20@
    | JudgeViolate     JudgePat [B.Named c]  -- ^ @|-v P \/x 10 \/y 20@
      deriving (Show)

instance (Ord c) => Eq (Judge c) where
    j1 == j2  =  compare j1 j2 == EQ
    j1 /= j2  =  compare j1 j2 /= EQ

instance (Ord c) => Ord (Judge c) where
    compare j1 j2 =
        let j1'  = sortJudgeTerms j1
            j2'  = sortJudgeTerms j2
            p1   = judgePat j1'
            p2   = judgePat j2'
            xs1  = judgeTerms j1'
            xs2  = judgeTerms j2'
        in compare p1 p2 `B.mappend` compare xs1 xs2

-- Apply function to each values
instance Functor Judge where
    fmap f j = judgeTermsMap (map g) j
        where g (n, v) = (n, f v)

-- | Name of judgement pattern, in other words, name of propositional function.
type JudgePat = String

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

judgeTermsMap :: ([B.Named a] -> [B.Named b]) -> Judge a -> Judge b
judgeTermsMap f (JudgeAffirm      p xs)      = JudgeAffirm    p (f xs)
judgeTermsMap f (JudgeDeny        p xs)      = JudgeDeny      p (f xs)
judgeTermsMap f (JudgeMultiDeny   p xs)      = JudgeMultiDeny p (f xs)
judgeTermsMap f (JudgeChange      p xs xs')  = JudgeChange    p (f xs) (f xs')
judgeTermsMap f (JudgeMultiChange p xs xs')  = JudgeChange    p (f xs) (f xs')
judgeTermsMap f (JudgeViolate     p xs)      = JudgeViolate   p (f xs)

-- | Prepend a term into judgement.
judgeCons :: B.Named c -> B.Map (Judge c)
judgeCons x = judgeTermsMap (x :)

-- | Sort terms in alphabetical order.
sortJudgeTerms :: (Ord c) => B.Map (Judge c)
sortJudgeTerms = judgeTermsMap B.sort


-- ----------------------  Logical quality

-- | Construct judgement from its pattern and terms.
type JudgeOf c = JudgePat -> [B.Named c] -> Judge c

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

putJudge :: (B.Write c) => Judge c -> IO ()
putJudge = hPutJudge IO.stdout

hPutJudge :: (B.Write c) => IO.Handle -> Judge c -> IO ()
hPutJudge h = IO.hPutStrLn h . writeDownJudge B.shortEmpty

