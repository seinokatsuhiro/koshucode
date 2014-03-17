{-# OPTIONS_GHC -Wall #-}

-- | Abort reasons

module Koshucode.Baala.Base.Abort.EachReason
( -- * Analysis
  AbortAnalysis (..),
  -- * Base
  AbortBase (..),
  abortNotFound,
  divideByZero,
  heteroDecimal,
  notNumber,
  (<!!>),
) where

import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Abort.Message as B



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

data AbortBase
    = ACDivideByZero
    | ACUnmatchType   String
    | ACHeteroDecimal String String
    | ACNotFound      String
    | ASNotNumber  String
      deriving (Show, Eq, Ord)

instance B.AbortBy AbortBase where
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
        r (ASNotNumber _)       = "Can't read as number"

        d (ACDivideByZero)      = []
        d (ACUnmatchType s)     = [s]
        d (ACHeteroDecimal x y) = [x ++ " : " ++ y]
        d (ACNotFound key)      = [key]
        d (ASNotNumber s)       = [s]

ab :: AbortBase -> B.Ab a
ab = Left . B.abortBy

-- | Different decimal length
heteroDecimal :: String -> String -> B.Ab a
heteroDecimal a b = ab $ ACHeteroDecimal a b

-- | Different decimal length
notNumber :: String -> B.Ab a
notNumber = ab . ASNotNumber

-- | Divide by zero
divideByZero :: B.Ab a
divideByZero = ab ACDivideByZero

-- | Not found
abortNotFound :: String -> B.AbortReason
abortNotFound = B.abortBy . ACNotFound

-- | Lookup association list. This function may abort.
(<!!>) :: [B.Named b] -> String -> B.Ab b
(<!!>) assoc key = loop assoc where
    loop [] = Left $ abortNotFound key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs
