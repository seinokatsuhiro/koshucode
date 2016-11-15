{-# OPTIONS_GHC -Wall #-}

-- | Term types and related functions.

module Koshucode.Baala.Syntax.Symbol.Term
  ( -- * Term
    Term, term,

    -- * Term name
    TermName, TermPath, SignedTermName,
    stringTermName,
    termNameString, termPathString,

    -- * Term name tuple
    TermName2, TermName3,
    TermName4, TermName5, TermName6,

    -- * Present or new term
    termP, termN,
    termsP, termsN, termsPN,
  ) where

import qualified Koshucode.Baala.Overture   as O


-- ----------------------  Term

-- | Term type: pair of term name and content.
type Term c = (TermName, c)

-- | Create term.
--
--   >>> term "size" 10 :: Term Int
--   ("size", 10)
--
term :: TermName -> c -> Term c
term n c = (n, c)


-- ----------------------  Term name

-- | Name of term, e.g., @\"size\"@ for the term name @\/size@.
type TermName = String

-- | Path of term names, e.g., term name @\/r\/x@
--   is correspond to path @[\"r\", \"x\"]@.
type TermPath = [TermName]

-- | Term name with plus-minus sign, e.g., @+\/size@, @-\/size@, or @\/size@.
type SignedTermName = (Ordering, TermName)

-- | Decode term name from string.
--
--   >>> stringTermName "/a"
--   "a"
-- 
stringTermName :: String -> TermName
stringTermName ('/' : n) = n
stringTermName n         = n

-- | Encode term name into string.
--
--   >>> termNameString "size"
--   "/size"
--
termNameString :: TermName -> String
termNameString n = '/' : n

-- | Encode term path into string.
--
--   >>> termPathString ["r", "x"]
--   "/r/x"
--
termPathString :: TermPath -> String
termPathString = concatMap termNameString


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

