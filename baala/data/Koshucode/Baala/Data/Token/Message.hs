{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Token.Message
  ( -- * Abortable
    abToken,
    -- * Message
    forbiddenInput,
    unexpSect,
    unkAngleText,
  ) where

import qualified Koshucode.Baala.Base       as B

abToken :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abToken = B.abortable "token"

-- | Forbidden input
forbiddenInput :: String -> B.Ab a
forbiddenInput = Left . B.abortLine "Forbidden input"

-- | Unexpedted section delimiter
unexpSect :: [String] -> B.Ab a
unexpSect = Left . B.abortLines "Unexpedted section delimiter"

-- | Unknown bracket text
unkAngleText :: String -> B.Ab a
unkAngleText = Left . B.abortLine "Unknown bracket text"
