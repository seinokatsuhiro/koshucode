{-# OPTIONS_GHC -Wall #-}

-- | Abort reasons

module Koshucode.Baala.Base.Abort.EachReason
( -- * Base
  AbortBase (..),
  abortNotFound,
  divideByZero,
  heteroDecimal,
  notNumber,
  (<!!>),
) where

import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Abort.Message as B



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
                { B.abortReason = r a
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
