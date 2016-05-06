{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Assert.Message
  ( -- * Abortable
    abAssert,
    -- * Message
    unkOption,
  ) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Syntax  as S

abAssert :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abAssert = B.abortable "assert"

-- | Unknown option
unkOption :: S.ParaUnmatch String -> B.Ab a
unkOption un = Left $ B.abortLines "Unknown option" detail where
    detail = case un of
               S.ParaOutOfRange n p  -> ["Positional parameter out of range",
                                         "Expect " ++ expect p ++
                                         ", but actural " ++ show n]
               S.ParaUnknown  ns     -> ["Unknown parameter name", unwords ns]
               S.ParaMissing  ns     -> ["Missing parameter name", unwords ns]
               S.ParaMultiple ns     -> ["Repeated parameter name", unwords ns]

    expect (S.ParaPosMin n) = "minimum " ++ show n
    expect (S.ParaPosRange m n)
        | m == n      = "just " ++ show n
        | m == 0      = "maximum " ++ show n
        | otherwise   = "between " ++ show m ++ " and " ++ show n

