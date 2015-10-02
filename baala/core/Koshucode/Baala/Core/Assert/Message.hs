{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Assert.Message
  ( -- * Abortable
    abAssert,
    -- * Message
    unkOption,
  ) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Data as D


abAssert :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abAssert = B.abortable "assert"

-- | Unknown option
unkOption :: D.ParaUnmatch String -> B.Ab a
unkOption un = Left $ B.abortLines "Unknown option" detail where
    detail = case un of
               D.ParaOutOfRange n p  -> ["Positional parameter out of range",
                                         "Expect " ++ expect p ++
                                         ", but actural " ++ show n]
               D.ParaUnknown  ns     -> ["Unknown parameter name", unwords ns]
               D.ParaMissing  ns     -> ["Missing parameter name", unwords ns]
               D.ParaMultiple ns     -> ["Repeated parameter name", unwords ns]

    expect (D.ParaPosJust n)     = "just "    ++ show n
    expect (D.ParaPosMin  n)     = "minimum " ++ show n
    expect (D.ParaPosMax  n)     = "maximum " ++ show n
    expect (D.ParaPosRange m n)  = "between " ++ show m ++ " and " ++ show n

