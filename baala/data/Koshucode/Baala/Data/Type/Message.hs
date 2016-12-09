{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Type.Message
  ( -- * Abortable
    -- * Message
    divideByZero,
    heteroDecimal,
    notDate,
    tooLargeDigit,
    notNumber,
    abortEncodables,
  ) where

import qualified Koshucode.Baala.Overture   as O
import qualified Koshucode.Baala.Base       as B

-- | Divide by zero
divideByZero :: B.Ab a
divideByZero = Left $ B.abortBecause "Divide by zero"

-- | Different decimal length
heteroDecimal :: (B.MixEncode c) => c -> c -> B.Ab a
heteroDecimal a b = Left $ abortEncodables "Different decimal length" [a, b]

-- | Can't read as date
notDate :: Integer -> Int -> Int -> B.Ab a
notDate y m d = Left $ B.abortLines "Can't read as date"
                [ "/year  " ++ show y
                , "/month " ++ show m
                , "/day   " ++ show d]

-- | Too large digit
tooLargeDigit :: String -> B.Ab a
tooLargeDigit = Left . B.abortLine "Too large digit"

-- | Can't read as number
notNumber :: String -> B.Ab a
notNumber = Left . B.abortLine "Can't read as number"

-- | Abort reason with encodable values.
abortEncodables :: (B.MixEncode c) => String -> [c] -> B.AbortReason
abortEncodables msg cs = B.abortLines msg (zipWith f (O.ints 1) cs) where
    f i c = "#" ++ show i ++ " = " ++ B.encode c
          
