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



-- ----------------------  Abortable process

-- | Push source information when process is aborted.
abortable :: String -> [B.Token] -> B.Map (B.Ab b)
abortable tag src = either (Left . pushSource tag src) Right

-- | Same as 'abortable' except for using 'B.TokenListing'
--   instead of list of 'B.Token'.
abortableFrom :: (B.TokenListing src) => String -> src -> B.Map (B.Ab b)
abortableFrom tag src = abortable tag $ B.tokenListing src

-- | Same as 'abortable' except for using 'B.TokenTree'
--   instead of list of 'B.Token'.
abortableTree :: String -> B.TokenTree -> B.Map (B.Ab b)
abortableTree tag tree = abortable tag $ treeToken tree

-- | Same as 'abortable' except for using list of 'B.TokenTree'
--   instead of list of 'B.Token'.
abortableTrees :: String -> [B.TokenTree] -> B.Map (B.Ab b)
abortableTrees tag trees = abortable tag $ treeToken (B.treeWrap trees)

abortableSourced :: String -> (a -> B.Ab b) -> B.Sourced a -> B.Ab (B.Sourced b)
abortableSourced tag f (B.Sourced src x) =
    abortable tag src $ do
      y <- f x
      Right $ B.Sourced src y

treeToken :: B.CodeTree a -> [a]
treeToken = B.front . B.untree

pushSource :: String -> [B.Token] -> B.Map B.AbortReason
pushSource name src1 abort@B.AbortReason { B.abortSource = src2 } =
    abort { B.abortSource = namedSrc1 ++ src2 }
    where namedSrc1 = map (name,) src1

