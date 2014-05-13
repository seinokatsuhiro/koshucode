{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Abortable process

module Koshucode.Baala.Base.Abort.Abortable
( abortable,
  abortableFrom,
  abortableSourced,
) where

import qualified Koshucode.Baala.Base.Prelude        as B
import qualified Koshucode.Baala.Base.Token          as B
import qualified Koshucode.Baala.Base.Abort.Reason   as B


-- | Push source information when process is aborted.
abortable :: String -> [B.Token] -> B.Map (B.Ab b)
abortable tag toks = either (Left . push tag toks) Right

-- | Same as 'abortable' except for using 'B.TokenList'
--   instead of list of 'B.Token'.
abortableFrom :: (B.TokenList src) => String -> src -> B.Map (B.Ab b)
abortableFrom tag = abortable tag . B.tokenList

abortableSourced :: String -> (a -> B.Ab b) -> B.Sourced a -> B.Ab (B.Sourced b)
abortableSourced tag f (B.Sourced toks x) =
    abortable tag toks $ do
      y <- f x
      Right $ B.Sourced toks y

push :: String -> [B.Token] -> B.Map B.AbortReason
push tag toks abort@B.AbortReason { B.abortSource = src } =
     case toks of
       []        -> abort
       [tok]     -> abort { B.abortSource = (tag, tok) : src }
       (tok : _) -> push tag [tok] abort

