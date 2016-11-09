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
import qualified Koshucode.Baala.Syntax        as S
import qualified Koshucode.Baala.Data.Message  as Msg

-- | Abortable scope for lexmap.
abLexmap :: [S.TTree] -> B.MapAb b
abLexmap = Msg.abortableTrees "lexmap"

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

