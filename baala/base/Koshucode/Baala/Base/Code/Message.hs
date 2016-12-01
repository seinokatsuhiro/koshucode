{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Base.Code.Message
  ( -- * Abortable
    abCode,
    -- * Message
    extraCloseBracket,
    extraOpenBracket,
  ) where

import qualified Koshucode.Baala.Base.Abort          as B

-- | Abortable scope for code.
abCode :: (B.GetCodePos cp) => B.Abortable cp b
abCode = B.abortable "code"

-- | Extra close bracket
extraCloseBracket :: B.Ab a
extraCloseBracket = Left $ B.abortBecause "Extra close bracket"

-- | Unclosed open bracket
extraOpenBracket :: B.Ab a
extraOpenBracket = Left $ B.abortBecause "Unclosed open bracket"

