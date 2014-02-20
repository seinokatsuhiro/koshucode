{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

{-| Bundle of abort reasons -}

module Koshucode.Baala.Base.Abort.Reason
( AbortReason (..),
  Ab, AbMap,
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



{-| Bundle of abort reasons. -}
data AbortReason
    = AbortIO                 B.AbortIO
    | AbortSyntax   [(String, B.Token)] B.AbortSyntax
    | AbortAnalysis [(String, B.Token)] B.AbortAnalysis
    | AbortCalc     [(String, B.Token)] B.AbortCalc
      deriving (Show, Eq, Ord)

instance B.Name AbortReason where
    name = B.abortSymbol

instance B.AbortReasonClass AbortReason where
    abortSymbol (AbortIO         a)  =  B.abortSymbol a
    abortSymbol (AbortSyntax   _ a)  =  B.abortSymbol a
    abortSymbol (AbortAnalysis _ a)  =  B.abortSymbol a
    abortSymbol (AbortCalc     _ a)  =  B.abortSymbol a

    abortClass  (AbortIO         a)  =  B.abortClass a
    abortClass  (AbortSyntax   _ a)  =  B.abortClass a
    abortClass  (AbortAnalysis _ a)  =  B.abortClass a
    abortClass  (AbortCalc     _ a)  =  B.abortClass a

    abortReason (AbortIO         a)  =  B.abortReason a
    abortReason (AbortSyntax   _ a)  =  B.abortReason a
    abortReason (AbortAnalysis _ a)  =  B.abortReason a
    abortReason (AbortCalc     _ a)  =  B.abortReason a

    abortDetail (AbortIO         a)  =  B.abortDetail a
    abortDetail (AbortSyntax   _ a)  =  B.abortDetail a
    abortDetail (AbortAnalysis _ a)  =  B.abortDetail a
    abortDetail (AbortCalc     _ a)  =  B.abortDetail a

    abortSource (AbortSyntax   s _)  =  source s
    abortSource (AbortAnalysis s _)  =  source s
    abortSource (AbortCalc     s _)  =  source s
    abortSource _ = []

{-| Abortable result, i.e., either of right result or abort reason. -}
type Ab b = Either AbortReason b

{-| Abortable mapping. -}
type AbMap b = b -> Ab b

source :: [(String, B.Token)] -> [(String, String)]
source = concatMap f . reverse where
    f :: (String, B.Token) -> [(String, String)]
    f (tag, token) = B.tokenPosDisplay tag $ B.tokenPos token

{-| Push source information when process is aborted.

    @ B.abortable src $ do ... @
    -}
abortable :: String -> [B.Token] -> B.Map (Ab b)
abortable name src = either (Left . pushSource name src) Right

{-| Same as 'ab' except for using 'B.TokenListing'
    instead of list of 'B.Token'. -}
abortableFrom :: (B.TokenListing src) => String -> src -> B.Map (Ab b)
abortableFrom name src = either (Left . pushSourceFrom name src) Right

abortableTree :: String -> B.TokenTree -> B.Map (Ab b)
abortableTree tag tree = abortable tag $ treeToken tree

abortableTrees :: String -> [B.TokenTree] -> B.Map (Ab b)
abortableTrees tag tree = abortable tag $ treeToken $ B.treeWrap tree

treeToken :: B.CodeTree a -> [a]
treeToken = B.front . B.untree

pushSource :: String -> [B.Token] -> B.Map AbortReason
pushSource name src1 abort =
    case abort of
      (AbortSyntax   src2 a) -> AbortSyntax   (namedSrc1 ++ src2) a
      (AbortAnalysis src2 a) -> AbortAnalysis (namedSrc1 ++ src2) a
      (AbortCalc     src2 a) -> AbortCalc     (namedSrc1 ++ src2) a
      _ -> abort
    where
      namedSrc1 = map (name,) src1

pushSourceFrom :: (B.TokenListing a) => String -> a -> B.Map AbortReason
pushSourceFrom name = pushSource name . B.tokenListing

sourcedAbMap :: (a -> Ab b) -> B.Sourced a -> Ab (B.Sourced b)
sourcedAbMap f (B.Sourced src x) =
    abortable "-" src $ do
      y <- f x
      Right $ B.Sourced src y

{-| Lookup association list. This function may abort. -}
(<!!>) :: [B.Named b] -> String -> Ab b
(<!!>) assoc key = loop assoc where
    loop [] = Left $ abortNotFound key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs

abortOperand :: String -> AbortReason
abortOperand = AbortAnalysis [] . B.AAUnexpectedOperand

abortTermIO :: [String] -> [Bool] -> AbortReason
abortTermIO ns here = AbortAnalysis [] $ B.AAUnrecTermIO ns here

abortNotFound :: String -> AbortReason
abortNotFound = AbortCalc [] . B.ACNotFound

{-| Stop on error @'bug in koshucode'@ -}
bug :: a
bug = error "bug in koshucode"

