{-# OPTIONS_GHC -Wall #-}

{-| Abort reasons -}

module Koshucode.Baala.Base.Abort.EachReason
( -- * I/O
  AbortIO (..),
  -- * Syntax
  AbortSyntax (..),
  -- * Analysis
  AbortAnalysis (..),
  -- * Calculation
  AbortCalc (..),
) where

import qualified Koshucode.Baala.Base.Abort.Class as B



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
    = ASAmbInfixes [String]     -- ^ Ambiguous infix operators
    | ASNotNumber  String       -- ^ Can't read as number
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
        (ASAmbInfixes _)     -> "Ambiguous infix operators"
        (ASNotNumber _)      -> "Can't read as number"
        (ASNotText _)        -> "Can't read as text"
        (ASOddRelation)      -> "Odd relation literal"
        (ASUnkClause)        -> "Unknown clause"
        (ASUnkCox _)         -> "Unknown expression"
        (ASUnkWord _)        -> "Unknown word"
        (ASUnresToken)       -> "Unresolved prefix"

    abortDetail a = case a of
        (ASAmbInfixes ops)   -> ops
        (ASNotNumber s)      -> [s]
        (ASNotText s)        -> [s]
        (ASOddRelation)      -> []
        (ASUnkClause)        -> []
        (ASUnkCox s)         -> [s]
        (ASUnkWord s)        -> [s]
        (ASUnresToken)       -> []


-- ----------------------  Analysis Error

data AbortAnalysis
    = AACheckTerms       [String]
    | AAMalformedOperand  String
    | AAMissingTermname
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
        (AAMissingTermname)     -> "Require termname"
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
        (AANoTerms ns)          -> [unwords ns]
        (AAOpeandDuplicate ns)  -> [unwords ns]
        (AAOpeandUnknown ns)    -> [unwords ns]
        (AAReqBoolean s)        -> [s]
        (AAReqFlatname s)       -> [s]
        (AAReqNewTerms ns)      -> [unwords ns]
        (AAUndefined x)         -> [x]
        (AAUnkCop op)           -> [op]
        (AAUnkRelmap op)        -> [op]
        _                       -> []


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

