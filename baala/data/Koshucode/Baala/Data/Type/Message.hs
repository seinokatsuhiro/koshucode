{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Message
  ( -- * Abortable
    -- * Message
    divideByZero,
    heteroDecimal,
    notDate,
    notNumber,
  ) where

import qualified Koshucode.Baala.Base       as B

-- | Divide by zero
divideByZero :: B.Ab a
divideByZero = Left $ B.abortBecause "Divide by zero"

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

