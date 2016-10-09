{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Syntax.Symbol.Term
  ( -- * Term name
    TermName, TermName2, TermName3,
    TermName4, TermName5, TermName6,
    TermPath, SignedTermName,
    showTermName, showTermPath,

    -- * Term
    Term, term,

    -- * Present or new term
    termP, termN, termPN,
    termsP, termsN, termsPN,
  ) where

import qualified Koshucode.Baala.Base   as B


-- ----------------------  Term name

-- | Name of term, e.g., @\"file\"@ for the term name @\/file@.
type TermName    = String
type TermName2   = (String, String)
type TermName3   = (String, String, String)
type TermName4   = (String, String, String, String)
type TermName5   = (String, String, String, String, String)
type TermName6   = (String, String, String, String, String, String)

-- | Path of term names, e.g., term name @\/r\/x@
--   is correspond to path @[\"r\", \"x\"]@.
type TermPath    = [TermName]

type SignedTermName = (Ordering, TermName)

showTermName :: TermName -> String
showTermName n = '/' : n

showTermPath :: TermPath -> String
showTermPath = concat . map showTermName


-- ----------------------  Term

type Term c      = (TermName, c)

term :: TermName -> c -> Term c
term n c = (n, c)


-- ----------------------  Present or new term

-- | Check present term.
termP :: B.Test Int
termP = (>= 0)

-- | Check new term.
termN :: B.Test Int
termN = (< 0)

termPN :: Int -> B.Test Int
termPN p n = termP p && termN n

termsP :: B.Test [Int]
termsP = all termP

termsN :: B.Test [Int]
termsN = all termN

termsPN :: [Int] -> B.Test [Int]
termsPN ps ns = termsP ps && termsN ns

