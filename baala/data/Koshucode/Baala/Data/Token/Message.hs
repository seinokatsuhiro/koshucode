{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Token.Message
  ( -- * Abortable
    abToken,
    -- * Message
    forbiddenInput,
    forbiddenTerm,
    quotNotEnd,
    unexpSect,
    unkAngleText,
  ) where

import qualified Koshucode.Baala.Base       as B

abToken :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abToken = B.abortable "token"

-- | Forbidden input
forbiddenInput :: String -> B.Ab a
forbiddenInput = Left . B.abortLine "Forbidden input"

-- | Forbidden term name
forbiddenTerm :: B.Ab a
forbiddenTerm = Left $ B.abortBecause "Forbidden term name"

-- | Quotation not end in line
quotNotEnd :: B.Ab a
quotNotEnd = Left $ B.abortBecause "Quotation not end in line"

-- | Unexpedted section delimiter
unexpSect :: [String] -> B.Ab a
unexpSect = Left . B.abortLines "Unexpedted section delimiter"

-- | Unknown bracket text
unkAngleText :: String -> B.Ab a
unkAngleText = Left . B.abortLine "Unknown bracket text"
