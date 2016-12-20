{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Core.Assert.Message
  ( -- * Abortable
    abAssert,
    -- * Message
    unkOption,
  ) where

import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Syntax    as S

-- | Abortable scope for assertion.
abAssert :: (B.GetCodePos cp) => B.Abortable cp b
abAssert = B.abortable "assert"

-- | Unknown option
unkOption :: S.ParaUnmatch String -> B.Ab a
unkOption un = B.leftLines "Unknown option" detail where
    detail = case un of
               S.ParaPos n p       -> ["Positional parameter out of range",
                                       "Expect " ++ expect p ++
                                       ", but actural " ++ show n]
               S.ParaUnknown  ns   -> ["Unknown parameter name", unwords ns]
               S.ParaMissing  ns   -> ["Missing parameter name", unwords ns]
               S.ParaMultiple ns   -> ["Repeated parameter name", unwords ns]

    expect (S.ParaItem     a _)    = "just " ++ show a
    expect (S.ParaItemOpt  a _ _)  = "minimum " ++ show a
    expect (S.ParaItemRest a _ _)  = "minimum " ++ show a
    expect (S.ParaMin      a)      = "minimum " ++ show a
    expect (S.ParaRange a b)
        | a == b      = "just "    ++ show b
        | a == 0      = "maximum " ++ show b
        | otherwise   = "between " ++ show a ++ " and " ++ show b

