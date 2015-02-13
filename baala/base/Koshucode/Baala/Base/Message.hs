{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Message
  ( -- * Abortables
    abToken,
    abTree,
  
    -- * Base package
    bug,
    adlib,
    adlibs,
    extraCloseBracket,
    extraOpenBracket,
    notFound,
    divideByZero,
    heteroDecimal,
    notDate,
    notNumber,
    notImplemented,
    (<!!>),
  
    -- * Tokenizer
    forbiddenInput,
    forbiddenTerm,
    quotNotEnd,
    reqFlatTerm,
    unexpSect,
    unkAngleText,
  ) where

import qualified Koshucode.Baala.Base.Abort    as B
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B


-- ----------------------  Abortables

abToken :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abToken = B.abortable "token"

abTree :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abTree = B.abortable "tree"


-- ----------------------  Base package

-- | BUG: reason
bug :: String -> B.Ab a
bug reason = Left $ B.abortBecause $ "BUG: " ++ reason

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

-- | Can't read as date
notDate :: Integer -> Int -> Int -> B.Ab a
notDate y m d = Left $ B.abortLines "Can't read as date"
                [ "/year  " ++ show y
                , "/month " ++ show m
                , "/day   " ++ show d]

-- | Can't read as number
notNumber :: String -> B.Ab a
notNumber = Left . B.abortLine "Can't read as number"

-- | Divide by zero
divideByZero :: B.Ab a
divideByZero = Left $ B.abortBecause "Divide by zero"

-- | Not found
notFound :: String -> B.Ab a
notFound = Left . B.abortLine "Not found"

-- | Not implemented
notImplemented :: String -> B.Ab a
notImplemented = Left . B.abortLine "Not implemented"

-- | Lookup association list. This function may abort.
(<!!>) :: [B.Named b] -> String -> B.Ab b
(<!!>) assoc key = loop assoc where
    loop [] = notFound key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs


-- --------------------  Tokenizer

-- | Forbidden input
forbiddenInput :: String -> B.Ab a
forbiddenInput = Left . B.abortLine "Forbidden input"

-- | Forbidden term name
forbiddenTerm :: B.Ab a
forbiddenTerm = Left $ B.abortBecause "Forbidden term name"

-- | Quotation not end in line
quotNotEnd :: B.Ab a
quotNotEnd = Left $ B.abortBecause "Quotation not end in line"

-- | Require flat term name
reqFlatTerm :: String -> B.Ab a
reqFlatTerm = Left . B.abortLine "Require flat term name"

-- | Unexpedted section delimiter
unexpSect :: [String] -> B.Ab a
unexpSect = Left . B.abortLines "Unexpedted section delimiter"

-- | Unknown bracket text
unkAngleText :: String -> B.Ab a
unkAngleText = Left . B.abortLine "Unknown bracket text"
