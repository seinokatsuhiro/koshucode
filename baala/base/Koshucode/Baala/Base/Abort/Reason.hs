{-# OPTIONS_GHC -Wall #-}

{-| Bundle of abort reasons -}

module Koshucode.Baala.Base.Abort.Reason
( AbortReason (..),
  Ab, AbMap,
  ab, abFrom,
  sourcedAbMap,
  (<!!>),
  abortMalformedOperand,
  abortNotFound,
  bug,
) where

import qualified Koshucode.Baala.Base.Prelude          as B
import qualified Koshucode.Baala.Base.Token            as B
import qualified Koshucode.Baala.Base.Abort.Class      as B
import qualified Koshucode.Baala.Base.Abort.EachReason as B



{-| Bundle of abort reasons. -}
data AbortReason
    = AbortIO                 B.AbortIO
    | AbortSyntax   [B.Token] B.AbortSyntax
    | AbortAnalysis [B.Token] B.AbortAnalysis
    | AbortCalc     [B.Token] B.AbortCalc
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

    abortSource (AbortSyntax   src _) = source src
    abortSource (AbortAnalysis src _) = source src
    abortSource (AbortCalc     src _) = source src
    abortSource _ = []

{-| Abortable result, i.e., either of right result or abort reason. -}
type Ab b = Either AbortReason b

{-| Abortable mapping. -}
type AbMap b = b -> Ab b

source :: [B.Token] -> [String]
source = concatMap f . reverse where
    f :: B.Token -> [String]
    f = B.tokenPosDisplay . B.tokenPos

{-| Push source information when process is aborted.

    @ B.ab src $ do ... @
    -}
ab :: [B.Token] -> B.Map (Ab b)
ab src = either (Left . pushSource src) Right

{-| Same as 'ab' except for using 'B.TokenListing'
    instead of list of 'B.Token'. -}
abFrom :: (B.TokenListing src) => src -> B.Map (Ab b)
abFrom src = either (Left . pushSourceFrom src) Right

pushSource :: [B.Token] -> B.Map AbortReason
pushSource src1 (AbortSyntax   src2 a) = AbortSyntax   (src1 ++ src2) a
pushSource src1 (AbortAnalysis src2 a) = AbortAnalysis (src1 ++ src2) a
pushSource src1 (AbortCalc     src2 a) = AbortCalc     (src1 ++ src2) a
pushSource _ a = a

pushSourceFrom :: (B.TokenListing a) => a -> B.Map AbortReason
pushSourceFrom = pushSource . B.tokenListing

sourcedAbMap :: (a -> Ab b) -> B.Sourced a -> Ab (B.Sourced b)
sourcedAbMap f (B.Sourced src x) =
    ab src $ do
      y <- f x
      Right $ B.Sourced src y

{-| Lookup association list. This function may abort. -}
(<!!>) :: [B.Named b] -> String -> Ab b
(<!!>) assoc key = loop assoc where
    loop [] = Left $ abortNotFound key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs

abortMalformedOperand :: String -> AbortReason
abortMalformedOperand = AbortAnalysis [] . B.AAMalformedOperand

abortNotFound :: String -> AbortReason
abortNotFound = AbortCalc [] . B.ACNotFound

{-| Stop on error @'bug in koshucode'@ -}
bug :: a
bug = error "bug in koshucode"

