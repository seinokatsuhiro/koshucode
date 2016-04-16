{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Token.Message
  ( -- * Abortable
    abToken,
    -- * Message
    expOrdSym,
    forbiddenInput,
    quotNotEnd,
    unexpSect,
    unkAngleText,
  ) where

import qualified Koshucode.Baala.Base       as B

abToken :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abToken = B.abortable "token"

-- | Expect ordinary symbol
expOrdSym :: B.Ab a
expOrdSym = Left $ B.abortBecause "Expect ordinary symbol"

-- | Forbidden input
forbiddenInput :: String -> B.Ab a
forbiddenInput = Left . B.abortLine "Forbidden input"

-- | Quotation not end in line
quotNotEnd :: B.Ab a
quotNotEnd = Left $ B.abortBecause "Quotation not end in line"

-- | Unexpedted section delimiter
unexpSect :: [String] -> B.Ab a
unexpSect = Left . B.abortLines "Unexpedted section delimiter"

-- | Unknown bracket text
unkAngleText :: String -> B.Ab a
unkAngleText = Left . B.abortLine "Unknown bracket text"
