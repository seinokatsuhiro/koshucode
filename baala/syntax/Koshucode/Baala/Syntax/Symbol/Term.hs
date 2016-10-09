{-# OPTIONS_GHC -Wall #-}

-- | Name of term.

module Koshucode.Baala.Syntax.Symbol.Term
  ( -- * Term name
    TermName, TermName2, TermName3,
    TermName4, TermName5, TermName6,
    TermPath, SignedTermName,
    showTermName, showTermPath,

    -- * Term
    Term, term,

    -- * Present or new term
    termP, termN, --termPN,
    termsP, termsN, termsPN,
  ) where

import qualified Koshucode.Baala.Base   as B


-- ----------------------  Term name

-- | Name of term, e.g., @\"size\"@ for the term name @\/size@.
type TermName = String

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

-- | Path of term names, e.g., term name @\/r\/x@
--   is correspond to path @[\"r\", \"x\"]@.
type TermPath = [TermName]

-- | Term name with plus-minus sign, e.g., @+\/size@, @-\/size@, or @\/size@.
type SignedTermName = (Ordering, TermName)

-- | Encode term name into string.
showTermName :: TermName -> String
showTermName n = '/' : n

-- | Encode term path into string.
--
--   >>> showTermPath ["g", "a"]
--   "/g/a"
--
showTermPath :: TermPath -> String
showTermPath = concatMap showTermName


-- ----------------------  Term

-- | Term type: pair of term name and content.
type Term c = (TermName, c)

-- | Create term.
--
--   >>> term "size" 10 :: Term Int
--   ("size",10)
--
term :: TermName -> c -> Term c
term n c = (n, c)


-- ----------------------  Present or new term

-- | Check present term.
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
termP :: B.Test Int
termP = (>= 0)

-- | Check new term.
termN :: B.Test Int
termN = (< 0)

-- Check present and new terms.
--termPN :: B.Test2 Int Int
--termPN p n = termP p && termN n

-- | Check all terms are present.
termsP :: B.Test [Int]
termsP = all termP

-- | Check all terms are new.
termsN :: B.Test [Int]
termsN = all termN

-- | Check present terms and new terms.
termsPN :: B.Test2 [Int] [Int]
termsPN ps ns = termsP ps && termsN ns

