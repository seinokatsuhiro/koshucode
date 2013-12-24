{-# OPTIONS_GHC -Wall #-}

{-| Abort symbol -}

module Koshucode.Baala.Base.Abort.Reason
( -- * Datatype
  Ab,
  AbMap,
  AbMap2,
  Abort,
  AbortOr,

  -- * Reason
  AbortReason (..),
  AbortIO (..),
  AbortSyntax (..),
  AbortAnalysis (..),
  abortMalformedOperand,
  AbortCalc (..),
  abortNotFound,
  (<!!>),
) where

import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Syntax        as B
import qualified Koshucode.Baala.Base.Token         as B
import qualified Koshucode.Baala.Base.Abort.Utility as B



-- ----------------------  Abort type

{-| Either of (1) right result, or (2) abort reason
    (without source code information). -}
type Ab b = Either AbortReason b

{-| Abortable mapping. -}
type AbMap b = b -> Ab b

type AbMap2 b a = b -> Ab a

{-| Abort reason and source information. -}
type Abort = B.AbortType AbortReason

{-| Either of (1) right result, or (2) abort reason with source. -}
type AbortOr b = B.AbortOrType AbortReason b


-- ----------------------  Abort reason

{-| Abort reasons -}
data AbortReason
    = AbortIO                     AbortIO
    | AbortSyntax   [B.TokenLine] AbortSyntax
    | AbortAnalysis [B.Token]     AbortAnalysis
    | AbortCalc     [B.Token]     AbortCalc
      deriving (Show, Eq, Ord)

instance B.Name AbortReason where
    name = B.abortSymbol

instance B.AbortReasonClass AbortReason where
    abortClass (AbortIO         a) = B.abortClass a
    abortClass (AbortSyntax   _ a) = B.abortClass a
    abortClass (AbortAnalysis _ a) = B.abortClass a
    abortClass (AbortCalc     _ a) = B.abortClass a

    abortSymbol (AbortIO         a) = B.abortSymbol a
    abortSymbol (AbortSyntax   _ a) = B.abortSymbol a
    abortSymbol (AbortAnalysis _ a) = B.abortSymbol a
    abortSymbol (AbortCalc     _ a) = B.abortSymbol a

    abortClause (AbortSyntax   src _) = map B.lineNumberContent src
    abortClause _ = []

    abortRelmap (AbortAnalysis ts _) = map (B.tokenPosDisplay . B.tokenPos) ts
    abortRelmap (AbortCalc     ts _) = map (B.tokenPosDisplay . B.tokenPos) ts
    abortRelmap _ = []

    abortReason a = case a of
        (AbortIO              a2) -> B.abortReason a2
        (AbortSyntax        _ a2) -> B.abortReason a2
        (AbortAnalysis      _ a2) -> B.abortReason a2
        (AbortCalc          _ a2) -> B.abortReason a2

    abortDetail a = case a of
        (AbortIO              a2) -> B.abortDetail a2
        (AbortSyntax        _ a2) -> B.abortDetail a2
        (AbortAnalysis      _ a2) -> B.abortDetail a2
        (AbortCalc          _ a2) -> B.abortDetail a2


-- ----------------------  I/O Error

data AbortIO
    = AIONoFile String
      deriving (Show, Eq, Ord)

instance B.AbortReasonClass AbortIO where
    abortClass _ = "I/O ERROR"

    abortReason a = case a of
        (AIONoFile _)  -> "File not found"

    abortDetail a = case a of
        (AIONoFile path)  -> [path]


-- ----------------------  Syntax Error

data AbortSyntax
    = ASNotNumber  String       -- ^ Can't read as number
    | ASNotText    String       -- ^ Can't read as text
    | ASOddRelation             -- ^ Odd relation literal
    | ASUnkClause               -- ^ Unknown clause
    | ASUnkCox     String       -- ^ Unknown expression
    | ASUnkWord    String       -- ^ Unknown word
    | ASUnresToken              -- ^ Unresolved prefix
      deriving (Show, Eq, Ord)

instance B.AbortReasonClass AbortSyntax where
    abortClass _ = "SYNTAX ERROR"

    abortReason a = case a of
        (ASNotNumber _) -> "Can't read as number"
        (ASNotText _)   -> "Can't read as text"
        (ASOddRelation) -> "Odd relation literal"
        (ASUnkClause)   -> "Unknown clause"
        (ASUnkCox _)    -> "Unknown expression"
        (ASUnkWord _)   -> "Unknown word"
        (ASUnresToken)  -> "Unresolved prefix"

    abortDetail a = case a of
        (ASNotNumber s) -> [s]
        (ASNotText s)   -> [s]
        (ASUnkClause)   -> []
        (ASUnkCox s)    -> [s]
        (ASUnkWord s)   -> [s]
        (ASUnresToken)  -> []
        (ASOddRelation) -> []


-- ----------------------  Analysis Error

data AbortAnalysis
    = AACheckTerms       [String]
    | AAMalformedOperand  String
    | AAMissingTermname   String
    | AANoTerms          [String]
    | AAOpeandDuplicate  [String]
    | AAOpeandUnknown    [String]
    | AAReqBoolean        String
    | AAReqFlatname       String
    | AAReqNewTerms      [String]
    | AAUndefined         String
    | AAUnkCop String
    | AAUnkRelmap String
      deriving (Show, Eq, Ord)

instance B.AbortReasonClass AbortAnalysis where
    abortClass _ = "ANALYSIS ERROR"

    abortReason a = case a of
        (AACheckTerms _)        -> "check-term failed"
        (AAMalformedOperand _)  -> "Malformed operand"
        (AAMissingTermname _)   -> "Require termname"
        (AANoTerms _)           -> "Input relation does not given terms"
        (AAOpeandDuplicate _)   -> "Dulicate operands"
        (AAOpeandUnknown _)     -> "Unknown operand"
        (AAReqBoolean _)        -> "Require boolean"
        (AAReqFlatname _)       -> "Require flatname"
        (AAReqNewTerms _)       -> "Require new term"
        (AAUndefined _)         -> "Undefined"
        (AAUnkCop _)            -> "Unknown content operator"
        (AAUnkRelmap _)         -> "Unknown relmap operator"

    abortDetail a = case a of
        (AACheckTerms ns)       -> [unwords ns]
        (AAMalformedOperand s)  -> [s]
        (AAMissingTermname s)   -> [s]
        (AANoTerms ns)          -> [unwords ns]
        (AAOpeandDuplicate ns)  -> [unwords ns]
        (AAOpeandUnknown ns)    -> [unwords ns]
        (AAReqBoolean s)        -> [s]
        (AAReqFlatname s)       -> [s]
        (AAReqNewTerms ns)      -> [unwords ns]
        (AAUndefined x)         -> [x]
        (AAUnkCop op)           -> [op]
        (AAUnkRelmap op)        -> [op]

abortMalformedOperand :: String -> AbortReason
abortMalformedOperand s = AbortAnalysis [] $ AAMalformedOperand s


-- ----------------------  Calc Error

data AbortCalc
    = ACDivideByZero
    | ACUnmatchType   String
    | ACHeteroDecimal String String
    | ACNotFound      String
      deriving (Show, Eq, Ord)

instance B.AbortReasonClass AbortCalc where
    abortClass _ = "CALC ERROR"

    abortReason a = case a of
        (ACDivideByZero)      -> "Divide by zero"
        (ACUnmatchType _)     -> "Type unmatch"
        (ACHeteroDecimal _ _) -> "Different decimal length"
        (ACNotFound _)        -> "Not found"

    abortDetail a = case a of
        (ACDivideByZero)      -> []
        (ACUnmatchType s)     -> [s]
        (ACHeteroDecimal x y) -> [x ++ " : " ++ y]
        (ACNotFound key)      -> [key]

abortNotFound :: String -> AbortReason
abortNotFound key = AbortCalc [] $ ACNotFound key

{-| Lookup association list.
    This function may abort on AbortLookup. -}
(<!!>) :: [B.Named a] -> String -> Ab a
(<!!>) assoc key = loop assoc where
    loop [] = Left $ abortNotFound key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs

