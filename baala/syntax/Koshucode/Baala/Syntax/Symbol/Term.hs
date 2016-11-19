{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term types and related functions.

module Koshucode.Baala.Syntax.Symbol.Term
  ( -- * Term
    Term, term,

    -- * Term name
    TermName, TermPath,
    ToTermName (..),
    stringTermName,
    termNameString, termPathString,
    termNameContent,
    termNameSign, termNameAltSign,
    orderingTermName,

    -- * Term name tuple
    TermName2, TermName3,
    TermName4, TermName5, TermName6,

    -- * Term index
    TermIndex,
    termP, termN,
    termsP, termsN, termsPN,
  ) where

import qualified Data.String                as S
import qualified Data.Text                  as Tx
import qualified Data.Text.Lazy             as Tz
import qualified Koshucode.Baala.Overture   as O


-- ----------------------  Term

-- | Term type: pair of term name and content.
type Term c = (TermName, c)

-- | Create term.
--
--   >>> term "size" 10 :: Term Int
--   (TermName "size", 10)
--
term :: (ToTermName n) => n -> c -> Term c
term n c = (toTermName n, c)


-- ----------------------  Term name

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

-- | Decode term name from string.
--
--   >>> stringTermName "/a"
--   TermName "a"
-- 
stringTermName :: String -> TermName
stringTermName ('/' : n) = TermName EQ n
stringTermName n         = TermName EQ n

-- | Encode term name into string.
--
--   >>> termNameString $ stringTermName "/size"
--   "/size"
--
termNameString :: TermName -> String
termNameString (TermName EQ n) = '/' : n
termNameString (TermName GT n) = '+' : '/' : n
termNameString (TermName LT n) = '-' : '/' : n

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


-- ----------------------  Term name tuple

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


-- ----------------------  Present or new term

-- | Term name and index.
type TermIndex = (TermName, Int)

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
termP :: O.Test Int
termP = (>= 0)

-- | Test new term.
termN :: O.Test Int
termN = (< 0)

-- | Test all terms are present.
termsP :: O.Test [Int]
termsP = all termP

-- | Test all terms are new.
--
--   >>> termsN [-1, -1]
--   True
--
termsN :: O.Test [Int]
termsN = all termN

-- | Test present and new terms.
--
--   >>> termsPN [1,2] [-1]
--   True
--
--   >>> termsPN [1,2] [-1,0]
--   False
--
termsPN :: O.Test2 [Int] [Int]
termsPN ps ns = termsP ps && termsN ns

