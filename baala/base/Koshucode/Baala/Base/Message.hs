{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Message
( -- * Abortables
  abToken,

  -- * Base package
  adlib,
  adlibs,
  extraCloseBracket,
  extraOpenBracket,
  notFound,
  divideByZero,
  heteroDecimal,
  notNumber,
  (<!!>),

  -- * Tokenizer
  forbiddenText,
  unkBracketText,
) where

import qualified Koshucode.Baala.Base.Abort    as B
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B

abToken :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abToken = B.abortable "token"

-- | AD-LIB: reason
adlib :: String -> B.Ab a
adlib reason = Left $ B.abortBecause $ "AD-LIB: " ++ reason

adlibs :: [String] -> B.Ab a
adlibs = Left . B.abortLines "AD-LIB"

-- | Extra close bracket
extraCloseBracket :: B.Ab a
extraCloseBracket = Left $ B.abortBecause "Extra close bracket"

-- | Unclosed open bracket
extraOpenBracket :: B.Ab a
extraOpenBracket = Left $ B.abortBecause "Unclosed open bracket"

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

-- | Forbidden text
forbiddenText :: String -> B.Ab a
forbiddenText = Left . B.abortLine "Forbidden text"

-- | Unknown bracket
unkBracketText :: String -> B.Ab a
unkBracketText = Left . B.abortLine "Unknown bracket text"

