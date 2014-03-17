{-# OPTIONS_GHC -Wall #-}

-- | Abort reasons

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

instance B.AbortBy AbortIO where
    abortBy a = B.AbortReason
                { B.abortSymbol = B.abortSymbolGet a
                , B.abortReason = r a
                , B.abortDetail = d a
                , B.abortSource = []
                } where

        r (AIONoFile _)     = "File not found"

        d (AIONoFile path)  = [path]



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

instance B.AbortBy AbortSyntax where
    abortBy a = B.AbortReason
                { B.abortSymbol = B.abortSymbolGet a
                , B.abortReason = r a
                , B.abortDetail = d a
                , B.abortSource = []
                } where

        r (ASAmbInfixes _)     = "Ambiguous infix operators"
        r (ASNotNumber _)      = "Can't read as number"
        r (ASNotText _)        = "Can't read as text"
        r (ASOddRelation)      = "Odd relation literal"
        r (ASUnkClause)        = "Unknown clause"
        r (ASUnkCox _)         = "Unknown expression"
        r (ASUnkWord _)        = "Unknown word"
        r (ASUnresToken)       = "Unresolved prefix"

        d (ASAmbInfixes ops)   = ops
        d (ASNotNumber s)      = [s]
        d (ASNotText s)        = [s]
        d (ASUnkCox s)         = [s]
        d (ASUnkWord s)        = [s]
        d _                    = []



-- ----------------------  Analysis Error

data AbortAnalysis
    = AACheckTerms       [String]
    | AAUnexpectedOperand String
    | AAReqTermName
    | AANoTerms          [String]
    | AAOperandNotFound
    | AAReqBoolean        String
    | AAReqFlatname       String
    | AAReqNewTerms      [String]
    | AAUndefined         String
    | AAUnkCop String
    | AAUnkRelmap String
    | AAUnrecTermIO      [String] [Bool]
      deriving (Show, Eq, Ord)

instance B.AbortBy AbortAnalysis where
    abortBy a = B.AbortReason
                { B.abortSymbol = B.abortSymbolGet a
                , B.abortReason = r a
                , B.abortDetail = d a
                , B.abortSource = []
                } where

        r (AACheckTerms _)        = "check-term failed"
        r (AAUnexpectedOperand _) = "Unexpected operand"
        r (AAReqTermName)         = "Require termn ame"
        r (AANoTerms _)           = "Input relation does not given terms"
        r (AAOperandNotFound)     = "Operand not found"
        r (AAReqBoolean _)        = "Require boolean"
        r (AAReqFlatname _)       = "Require flatname"
        r (AAReqNewTerms _)       = "Require new term"
        r (AAUnrecTermIO _ _)     = "Unrecognized term I/O"
        r (AAUndefined _)         = "Undefined"
        r (AAUnkCop _)            = "Unknown content operator"
        r (AAUnkRelmap _)         = "Unknown relmap operator"

        d (AACheckTerms ns)       = [unwords ns]
        d (AAUnexpectedOperand s) = [s]
        d (AANoTerms ns)          = [unwords ns]
        d (AAOperandNotFound)     = []
        d (AAReqBoolean s)        = [s]
        d (AAReqFlatname s)       = [s]
        d (AAReqNewTerms ns)      = [unwords ns]
        d (AAUndefined x)         = [x]
        d (AAUnkCop op)           = [op]
        d (AAUnkRelmap op)        = [op]
        d (AAUnrecTermIO ns here) = [termIOText ns here]
        d _                       = []

termIOText :: [String] -> [Bool] -> String
termIOText ns here = unwords $ map termIO $ zip ns here

termIO :: (String, Bool) -> String
termIO (n, True)  = n ++ " in"
termIO (n, False) = n ++ " out"


-- ----------------------  Calc Error

data AbortCalc
    = ACDivideByZero
    | ACUnmatchType   String
    | ACHeteroDecimal String String
    | ACNotFound      String
      deriving (Show, Eq, Ord)

instance B.AbortBy AbortCalc where
    abortBy a = B.AbortReason
                { B.abortSymbol = B.abortSymbolGet a
                , B.abortReason = r a
                , B.abortDetail = d a
                , B.abortSource = []
                } where

        r (ACDivideByZero)      = "Divide by zero"
        r (ACUnmatchType _)     = "Type unmatch"
        r (ACHeteroDecimal _ _) = "Different decimal length"
        r (ACNotFound _)        = "Not found"

        d (ACDivideByZero)      = []
        d (ACUnmatchType s)     = [s]
        d (ACHeteroDecimal x y) = [x ++ " : " ++ y]
        d (ACNotFound key)      = [key]

