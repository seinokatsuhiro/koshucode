{-# OPTIONS_GHC -Wall #-}

{-| Abort symbol -}

module Koshucode.Baala.Base.Abort.Reason
( -- * Datatype
  Ab,
  AbMap,

  -- * Abort reason
  AbortReason (..),
  abortPushToken,
  abortPushTokenFrom,
  abortMalformedOperand,
  abortNotFound,
  (<!!>),
) where

import qualified Koshucode.Baala.Base.Prelude          as B
import qualified Koshucode.Baala.Base.Token            as B
import qualified Koshucode.Baala.Base.Abort.Utility    as B
import qualified Koshucode.Baala.Base.Abort.EachReason as B



-- ----------------------  Abort type

{-| Either of (1) right result, or (2) abort reason. -}
type Ab b = Either AbortReason b

{-| Abortable mapping. -}
type AbMap b = b -> Ab b


-- ----------------------  Abort reason

{-| Abort reasons -}
data AbortReason
    = AbortIO                 B.AbortIO
    | AbortSyntax   [B.Token] B.AbortSyntax
    | AbortAnalysis [B.Token] B.AbortAnalysis
    | AbortCalc     [B.Token] B.AbortCalc
      deriving (Show, Eq, Ord)

instance B.Name AbortReason where
    name = B.abortSymbol

instance B.AbortReasonClass AbortReason where
    abortClass  (AbortIO         a)  =  B.abortClass a
    abortClass  (AbortSyntax   _ a)  =  B.abortClass a
    abortClass  (AbortAnalysis _ a)  =  B.abortClass a
    abortClass  (AbortCalc     _ a)  =  B.abortClass a

    abortSymbol (AbortIO         a)  =  B.abortSymbol a
    abortSymbol (AbortSyntax   _ a)  =  B.abortSymbol a
    abortSymbol (AbortAnalysis _ a)  =  B.abortSymbol a
    abortSymbol (AbortCalc     _ a)  =  B.abortSymbol a

    abortReason (AbortIO         a)  =  B.abortReason a
    abortReason (AbortSyntax   _ a)  =  B.abortReason a
    abortReason (AbortAnalysis _ a)  =  B.abortReason a
    abortReason (AbortCalc     _ a)  =  B.abortReason a

    abortDetail (AbortIO         a)  =  B.abortDetail a
    abortDetail (AbortSyntax   _ a)  =  B.abortDetail a
    abortDetail (AbortAnalysis _ a)  =  B.abortDetail a
    abortDetail (AbortCalc     _ a)  =  B.abortDetail a

    abortSource (AbortSyntax   ts _) = source ts
    abortSource (AbortAnalysis ts _) = source ts
    abortSource (AbortCalc     ts _) = source ts
    abortSource _ = []

source :: [B.Token] -> [String]
source = concatMap tokenSource . reverse

tokenSource :: B.Token -> [String]
tokenSource = B.tokenPosDisplay . B.tokenPos

abortPushToken :: [B.Token] -> B.Map AbortReason
abortPushToken t1 (AbortSyntax   t2 a) = AbortSyntax   (t1 ++ t2) a
abortPushToken t1 (AbortAnalysis t2 a) = AbortAnalysis (t1 ++ t2) a
abortPushToken t1 (AbortCalc     t2 a) = AbortCalc     (t1 ++ t2) a
abortPushToken _ a = a

abortPushTokenFrom :: (B.TokenListing a) => a -> B.Map AbortReason
abortPushTokenFrom = abortPushToken . B.tokenListing

abortMalformedOperand :: String -> AbortReason
abortMalformedOperand s = AbortAnalysis [] $ B.AAMalformedOperand s

abortNotFound :: [B.Token] -> String -> AbortReason
abortNotFound src key = AbortCalc src $ B.ACNotFound key

{-| Lookup association list.
    This function may abort on AbortLookup. -}
(<!!>) :: [B.Named a] -> String -> Ab a
(<!!>) assoc key = loop assoc where
    loop [] = Left $ abortNotFound [] key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs

