{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term types and related functions.

module Koshucode.Baala.Syntax.Symbol.Term
  ( -- * Term
    RawJudge,
    JudgeTerm,
    JudgeClass,
    Term, term,

    -- * Term name
    TermName, TermPath,
    ToTermName (..),
    TextualTermName,
    enslash,
    textualTermName, stringTermNames,
    termNameString, termPathString,
    termNameContent,
    termNameSign, termNameAltSign,
    orderingTermName,

    -- * Term name tuple
    TermName2, TermName3,
    TermName4, TermName5, TermName6,

    -- * Term index
    TermIndex, IndexTerm,
    termP, termN,
    termsP, termsN, termsPN,
  ) where

import qualified Data.String                as S
import qualified Koshucode.Baala.Overture   as O


-- ============================================  Term

-- | Judgement material.
type RawJudge c = (JudgeClass, [Term c])

-- | Term with judgement class.
type JudgeTerm c = (JudgeClass, Term c)

-- | Name of judgement class, in other words, name of propositional function.
type JudgeClass = String

-- | Term type: pair of term name and content.
type Term c = (TermName, c)

-- | Create term.
--
--   >>> term "size" 10 :: Term Int
--   (TermName EQ "size", 10)
--
term :: (ToTermName n) => n -> c -> Term c
term n c = (toTermName n, c)


-- ============================================  Term name

-- | Name of term, e.g., @\"size\"@ for the term name @\/size@.
data TermName =
    TermName Ordering String
    deriving (Show, Eq, Ord)

instance S.IsString TermName where
    fromString = toTermName

-- | Path of term names, e.g., term name @\/r\/x@
--   is correspond to path @[\"r\", \"x\"]@.
type TermPath = [TermName]

-- | Convert to term name.
--
--   >>> toTermName "/a"
--   TermName EQ "a"
--
--   >>> toTermName "+/a"
--   TermName GT "a"
-- 
class (Ord a) => ToTermName a where
    toTermName :: a -> TermName

instance ToTermName TermName where
    toTermName = id

-- | Remove leading slash character.
instance ToTermName String where
    toTermName = textualTermName

-- | Strict text.
instance ToTermName O.Tx where
    toTermName = textualTermName

-- | Lazy text.
instance ToTermName O.Tz where
    toTermName = textualTermName

-- | Integer term name.
instance ToTermName Int where
    toTermName = toTermName . show

-- | 'Textual' and 'ToTermName' values.
class (O.Textual t, ToTermName t) => TextualTermName t

instance TextualTermName String
instance TextualTermName O.Tx
instance TextualTermName O.Tz

-- | Add necessary slash character indicating term name.
--
--   >>> enslash <$> ["foo", "/bar", "+/baz", "-/qux"]
--   ["/foo", "/bar", "+/baz", "-/qux"]
--
enslash :: (O.Textual t) => O.Map t
enslash n@(O.cut  -> O.Jp '/' _)        = n
enslash n@(O.cut2 -> O.Jp2 '+' '/' _)   = n
enslash n@(O.cut2 -> O.Jp2 '-' '/' _)   = n
enslash n                               = '/' O.<:> n

-- | Decode term name from string.
{-# DEPRECATED textualTermName "Use 'toTermName' instead." #-}
textualTermName :: (O.Textual t) => t -> TermName
textualTermName (O.cut  -> O.Jp '/' n)       = TermName EQ $ O.tString n
textualTermName (O.cut2 -> O.Jp2 '+' '/' n)  = TermName GT $ O.tString n
textualTermName (O.cut2 -> O.Jp2 '-' '/' n)  = TermName LT $ O.tString n
textualTermName n                            = TermName EQ $ O.tString n

-- | Convert string to multiple term names.
--
--   >>> stringTermNames "/a /b /c"
--   [TermName EQ "a", TermName EQ "b", TermName EQ "c"]
--
stringTermNames :: String -> [TermName]
stringTermNames = fmap toTermName . words

-- | Encode term name into string.
--
--   >>> termNameString $ toTermName "/size"
--   "/size"
--
termNameString :: TermName -> String
termNameString (TermName EQ n) = enslash n
termNameString (TermName GT n) = '+' O.<:> enslash n
termNameString (TermName LT n) = '-' O.<:> enslash n

-- | Extract internal name.
--
--   >>> termNameContent $ toTermName "/size"
--   "size"
--
termNameContent :: TermName -> String
termNameContent (TermName _ n) = n

-- | Encode term path into string.
--
--   >>> termPathString [toTermName "r", toTermName "x"]
--   "/r/x"
--
termPathString :: TermPath -> String
termPathString = concatMap termNameString

-- | Sign of term name.
termNameSign :: TermName -> Ordering
termNameSign (TermName sign _) = sign

-- | Alter sign of term name.
termNameAltSign :: Ordering -> O.Map TermName
termNameAltSign sign (TermName _ n) = TermName sign n

-- | Convert to ordering pair.
orderingTermName :: TermName -> (Ordering, TermName)
orderingTermName n@(TermName sign _) = (sign, n)


-- ============================================  Term name tuple

-- | Tuple of 2 term names.
type TermName2 = (TermName, TermName)

-- | Tuple of 3 term names.
type TermName3 = (TermName, TermName, TermName)

-- | Tuple of 4 term names.
type TermName4 = (TermName, TermName, TermName, TermName)

-- | Tuple of 5 term names.
type TermName5 = (TermName, TermName, TermName, TermName, TermName)

-- | Tuple of 6 term names.
type TermName6 = (TermName, TermName, TermName, TermName, TermName, TermName)


-- ============================================  Term index

-- | Term index.
type TermIndex = Int

-- | Term name and index.
type IndexTerm = (TermName, TermIndex)

-- | Test present term.
--
--   >>> termP 2
--   True
--
--   >>> termP 0
--   True
--
--   >>> termP (-1)
--   False
--
termP :: O.Test TermIndex
termP = (>= 0)

-- | Test new term.
termN :: O.Test TermIndex
termN = (< 0)

-- | Test all terms are present.
termsP :: O.Test [TermIndex]
termsP = all termP

-- | Test all terms are new.
--
--   >>> termsN [-1, -1]
--   True
--
termsN :: O.Test [TermIndex]
termsN = all termN

-- | Test present and new terms.
--
--   >>> termsPN [1,2] [-1]
--   True
--
--   >>> termsPN [1,2] [-1,0]
--   False
--
termsPN :: O.Test2 [TermIndex] [TermIndex]
termsPN ps ns = termsP ps && termsN ns

