{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Core.Lexmap.Message
  ( -- * Abortable
    abLexmap,
    -- * Message
    ambRelmap,
    reqGroup,
    unkRelmap,
  ) where

import qualified Koshucode.Baala.Base          as B

-- | Abortable scope for lexmap.
abLexmap :: (B.GetCodePos cp) => B.Abortable cp b
abLexmap = B.abortable "lexmap"

-- | Ambiguous relmaps
ambRelmap :: String -> [d] -> B.Ab a
ambRelmap name ds = Left $ B.abortLine "Ambiguous relmaps"
                         $ name ++ " (" ++ show (length ds) ++ ")"

-- | Require grouping paren
reqGroup :: B.Ab a
reqGroup = Left $ B.abortBecause "Require grouping parens"

-- | Unknown relmap operator
unkRelmap :: String -> B.Ab a
unkRelmap = Left . B.abortLine "Unknown relmap operator"

