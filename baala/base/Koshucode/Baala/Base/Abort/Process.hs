{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Abortable process

module Koshucode.Baala.Base.Abort.Process
( abortable,
  abortableFrom,
  abortableTree,
  abortableTrees,
  abortableSourced,
) where

import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax          as B
import qualified Koshucode.Baala.Base.Token           as B
import qualified Koshucode.Baala.Base.Abort.Report    as B



-- | Push source information when process is aborted.
abortable :: String -> [B.Token] -> B.Map (B.Ab b)
abortable tag toks = either (Left . push tag toks) Right

-- | Same as 'abortable' except for using 'B.TokenListing'
--   instead of list of 'B.Token'.
abortableFrom :: (B.TokenListing src) => String -> src -> B.Map (B.Ab b)
abortableFrom tag = abortable tag . B.tokenListing

-- | Same as 'abortable' except for using 'B.TokenTree'
--   instead of list of 'B.Token'.
abortableTree :: String -> B.TokenTree -> B.Map (B.Ab b)
abortableTree tag = abortable tag . B.untree

-- | Same as 'abortable' except for using list of 'B.TokenTree'
--   instead of list of 'B.Token'.
abortableTrees :: String -> [B.TokenTree] -> B.Map (B.Ab b)
abortableTrees tag = abortable tag . B.untrees

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

