{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Message
( -- * Base package
  notFound,
  divideByZero,
  heteroDecimal,
  notNumber,
  (<!!>),
) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Abort.Report as B

-- | Different decimal length
heteroDecimal :: String -> String -> B.Ab a
heteroDecimal a b = Left $ B.abortLines "Different decimal length" [a, b]

-- | Can't read as number
notNumber :: String -> B.Ab a
notNumber = Left . B.abortLine "Can't read as number"

-- | Divide by zero
divideByZero :: B.Ab a
divideByZero = Left $ B.abortBecause "Divide by zero"

-- | Not found
notFound :: String -> B.Ab a
notFound = Left . B.abortLine "Not found"

-- | Lookup association list. This function may abort.
(<!!>) :: [B.Named b] -> String -> B.Ab b
(<!!>) assoc key = loop assoc where
    loop [] = notFound key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs

