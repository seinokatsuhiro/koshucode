{-# OPTIONS_GHC -Wall #-}

-- | Parened tree of tokens

module Koshucode.Baala.Base.Token.Abortable
(
  abortableTree,
  abortableTrees,
  abortTokens,
  abortableFrom,
  abortableSourced,
) where

import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax          as B
import qualified Koshucode.Baala.Base.Token.Token     as B
import qualified Koshucode.Baala.Base.Token.TokenTree as B



abortTokens :: String -> [B.Token] -> B.AbortReason
abortTokens reason tokens =
    (B.abortBecause reason) { B.abortDetail = map B.tokenContent tokens }

-- | Same as 'abortable' except for using 'B.TokenTree'
--   instead of list of 'B.Token'.
abortableTree :: String -> B.TokenTree -> B.Map (B.Ab b)
abortableTree tag = B.abortable tag . B.untree

-- | Same as 'abortable' except for using list of 'B.TokenTree'
--   instead of list of 'B.Token'.
abortableTrees :: String -> [B.TokenTree] -> B.Map (B.Ab b)
abortableTrees tag = B.abortable tag . B.untrees

-- | Same as 'abortable' except for using 'B.TokenList'
--   instead of list of 'B.Token'.
abortableFrom :: (B.TokenList src) => String -> src -> B.Map (B.Ab b)
abortableFrom tag = B.abortable tag . B.tokenList

abortableSourced :: String -> (a -> B.Ab b) -> B.Sourced a -> B.Ab (B.Sourced b)
abortableSourced tag f (B.Sourced toks x) =
    B.abortable tag toks $ do
      y <- f x
      Right $ B.Sourced toks y

