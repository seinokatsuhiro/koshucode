{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Syntax.Token.Message
  ( -- * Abortable
    abToken,
    -- * Message
    forbiddenInput,
    unexpSect,
    unkAngleText,
  ) where

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B

-- | Abortable scope for token.
abToken :: (B.GetCodePos cp) => B.Abortable cp b
abToken = B.abortable "token"

-- | Forbidden input
forbiddenInput :: (O.Textual t) => t -> B.Ab a
forbiddenInput = B.leftLine "Forbidden input" . O.tString

-- | Unexpedted section delimiter
unexpSect :: [String] -> B.Ab a
unexpSect = B.leftLines "Unexpedted section delimiter"

-- | Unknown bracket text
unkAngleText :: String -> B.Ab a
unkAngleText = B.leftLine "Unknown bracket text"
