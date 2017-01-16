{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term types and related functions.

module Koshucode.Baala.Syntax.Symbol.Term
  ( -- * Term
    RawJudge,
    JudgeClass,
    Term, term,

    -- * Term name
    TermName, TermPath,
    ToTermName (..),
    enslash,
    stringTermName, stringTermNames,
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
import qualified Data.Text                  as Tx
import qualified Data.Text.Lazy             as Tz
import qualified Koshucode.Baala.Overture   as O


-- ============================================  Term

-- | Judgement material.
type RawJudge c = (JudgeClass, [Term c])

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
class ToTermName a where
    toTermName :: a -> TermName

instance ToTermName TermName where
    toTermName = id

-- | Remove leading slash character.
instance ToTermName String where
    toTermName = stringTermName

-- | Strict text.
instance ToTermName Tx.Text where
    toTermName = toTermName . Tx.unpack

-- | Lazy text.
instance ToTermName Tz.Text where
    toTermName = toTermName . Tz.unpack

-- | Integer term name.
instance ToTermName Int where
    toTermName = toTermName . show

-- | Add necessary slash character indicating term name.
--
--   >>> enslash <$> ["foo", "/bar", "+/baz", "-/qux"]
--   ["/foo", "/bar", "+/baz", "-/qux"]
--
enslash :: O.Map String
enslash n@('/' : _)        = n
enslash n@('+' : '/' : _)  = n
enslash n@('-' : '/' : _)  = n
enslash n                  = '/' : n

-- | Decode term name from string.
--
--   >>> stringTermName "/a"
--   TermName EQ "a"
--
--   >>> stringTermName "+/a"
--   TermName GT "a"
-- 
stringTermName :: String -> TermName
stringTermName ('/' : n)        = TermName EQ n
stringTermName ('+' : '/' : n)  = TermName GT n
stringTermName ('-' : '/' : n)  = TermName LT n
stringTermName n                = TermName EQ n

-- | Convert string to multiple term names.
--
--   >>> stringTermNames "/a /b /c"
--   [TermName EQ "a", TermName EQ "b", TermName EQ "c"]
--
stringTermNames :: String -> [TermName]
stringTermNames = fmap stringTermName . words

-- | Encode term name into string.
--
--   >>> termNameString $ stringTermName "/size"
--   "/size"
--
termNameString :: TermName -> String
termNameString (TermName EQ n) = enslash n
termNameString (TermName GT n) = '+' : enslash n
termNameString (TermName LT n) = '-' : enslash n

-- | Extract internal name.
--
--   >>> termNameContent $ stringTermName "/size"
--   "size"
--
termNameContent :: TermName -> String
termNameContent (TermName _ n) = n

-- | Encode term path into string.
--
--   >>> termPathString [stringTermName "r", stringTermName "x"]
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

