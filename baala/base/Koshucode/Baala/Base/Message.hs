{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Message
( adlib,
  extraCloseParen,
  extraOpenParen,
  notFound,
  divideByZero,
  heteroDecimal,
  notNumber,
  (<!!>),
) where

import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Abort    as B


-- | AD-LIB: reason
adlib :: String -> B.Ab a
adlib reason = Left $ B.abortBecause $ "AD-LIB: " ++ reason

-- | Extra close paren
extraCloseParen :: B.Ab a
extraCloseParen = Left $ B.abortBecause "Extra close paren"

-- | Unclosed open paren
extraOpenParen :: B.Ab a
extraOpenParen = Left $ B.abortBecause "Unclosed open paren"

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

