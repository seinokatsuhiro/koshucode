{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Bundle of abort reasons

module Koshucode.Baala.Base.Abort.Reason
( Ab, AbMap,
  abortable, abortableFrom,
  abortableTree, abortableTrees,
  sourcedAbMap,
  (<!!>),
  abortOperand,
  abortTermIO,
  abortNotFound,
  bug,
) where

import qualified Koshucode.Baala.Base.Prelude          as B
import qualified Koshucode.Baala.Base.Syntax           as B
import qualified Koshucode.Baala.Base.Token            as B
import qualified Koshucode.Baala.Base.Abort.Class      as B
import qualified Koshucode.Baala.Base.Abort.EachReason as B



-- | Abortable result, i.e., either of right result or abort reason.
type Ab b = Either B.AbortReason b

-- | Abortable mapping.
type AbMap b = b -> Ab b

-- | Push source information when process is aborted.
--
--   @ B.abortable src $ do ... @
--
abortable :: String -> [B.Token] -> B.Map (Ab b)
abortable name src = either (Left . pushSource name src) Right

-- | Same as 'ab' except for using 'B.TokenListing'
--   instead of list of 'B.Token'.
abortableFrom :: (B.TokenListing src) => String -> src -> B.Map (Ab b)
abortableFrom name src = either (Left . pushSourceFrom name src) Right

abortableTree :: String -> B.TokenTree -> B.Map (Ab b)
abortableTree tag tree = abortable tag $ treeToken tree

abortableTrees :: String -> [B.TokenTree] -> B.Map (Ab b)
abortableTrees tag tree = abortable tag $ treeToken $ B.treeWrap tree

treeToken :: B.CodeTree a -> [a]
treeToken = B.front . B.untree

pushSource :: String -> [B.Token] -> B.Map B.AbortReason
pushSource name src1 abort@B.AbortReason { B.abortSource = src2 } =
    abort { B.abortSource = namedSrc1 ++ src2 }
    where
      namedSrc1 = map (name,) src1

pushSourceFrom :: (B.TokenListing a) => String -> a -> B.Map B.AbortReason
pushSourceFrom name = pushSource name . B.tokenListing

sourcedAbMap :: (a -> Ab b) -> B.Sourced a -> Ab (B.Sourced b)
sourcedAbMap f (B.Sourced src x) =
    abortable "-" src $ do
      y <- f x
      Right $ B.Sourced src y

-- | Lookup association list. This function may abort.
(<!!>) :: [B.Named b] -> String -> Ab b
(<!!>) assoc key = loop assoc where
    loop [] = Left $ abortNotFound key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs

abortOperand :: String -> B.AbortReason
abortOperand = B.abortBy . B.AAUnexpectedOperand

abortTermIO :: [String] -> [Bool] -> B.AbortReason
abortTermIO ns here = B.abortBy $ B.AAUnrecTermIO ns here

abortNotFound :: String -> B.AbortReason
abortNotFound = B.abortBy . B.ACNotFound

-- | Stop on error @'bug in koshucode'@
bug :: String -> a
bug msg = error $ "BUG DISCOVERED: " ++ msg

