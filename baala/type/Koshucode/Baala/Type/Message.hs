{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Type.Message
  ( -- * Abortable
    -- * Message
    divideByZero,
    heteroDecimal,
    notDate, notMonthlyDate, notWeeklyDate, notYearlyDate,
    tooLargeDigit,
    notNumber,
    abortEncodables,
    encodableLines,
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
notDate y m d = notMonthlyDate y m d

-- | Not monthly date
notMonthlyDate :: Integer -> Int -> Int -> B.Ab a
notMonthlyDate y m d =
    Left $ B.abortLines "Not monthly date"
             [ "/year  " ++ show y
             , "/month " ++ show m
             , "/day   " ++ show d]

-- | Not weekly date
notWeeklyDate :: Integer -> Int -> Int -> B.Ab a
notWeeklyDate y w d =
    Left $ B.abortLines "Not weekly date"
             [ "/year  " ++ show y
             , "/week  " ++ show w
             , "/day   " ++ show d]

-- | Not yearly date
notYearlyDate :: Integer -> Int -> B.Ab a
notYearlyDate y d =
    Left $ B.abortLines "Not yearly date"
             [ "/year  " ++ show y
             , "/day   " ++ show d]

-- | Too large digit
tooLargeDigit :: String -> B.Ab a
tooLargeDigit = Left . B.abortLine "Too large digit"

-- | Can't read as number
notNumber :: String -> B.Ab a
notNumber = Left . B.abortLine "Can't read as number"

-- | Abort reason with encodable values.
abortEncodables :: (B.MixEncode c) => String -> [c] -> B.AbortReason
abortEncodables msg cs = B.abortLines msg $ encodableLines cs

-- | Numbered lines of encodables.
encodableLines :: (B.MixEncode c) => [c] -> [String]
encodableLines = zipWith f (O.ints 1) where
    f i c = "#" ++ show i ++ " = " ++ B.encode c
