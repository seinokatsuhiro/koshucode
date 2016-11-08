{-# OPTIONS_GHC -Wall #-}

-- | Encode and decode integers.

module Koshucode.Baala.Overture.Text.Integer
  ( -- * Decoder
    readInt, readInteger,
    stringHex, stringHexInt, stringHexInteger,
    -- * Encoder
    intLowerHexString, intUpperHexString,
  ) where

import qualified Data.Char                      as Ch
import qualified Numeric                        as Num


-- ----------------------  Decoder

readMaybe :: (String -> [(a, String)]) -> String -> Maybe a
readMaybe f s = case f s of
                  [(x, "")] -> Just x
                  _         -> Nothing

-- | Decode decimal integer.
readDec :: (Eq n, Num n) => String -> Maybe n
readDec = readMaybe Num.readDec

-- | Decode decimal integer as 'Int'.
--
--   >>> readInt "12"
--   Just 12
--
--   >>> readInt "12.3"
--   Nothing
--
--   >>> readInt "12345678901234567890"
--   Just (-6101065172474983726)
--
readInt :: String -> Maybe Int
readInt = readDec

-- | Decode decimal integer as 'Integer'.
--
--   >>> readInteger "12345678901234567890"
--   Just 12345678901234567890
--
readInteger :: String -> Maybe Integer
readInteger = readDec

-- | Decode hexadecimal digits.
stringHex :: (Eq n, Num n) => String -> Maybe n
stringHex = readMaybe Num.readHex

-- | Decode hexadecimal digits as 'Int'.
--
--   >>> stringHexInt "0F"
--   Just 15
--
stringHexInt :: String -> Maybe Int
stringHexInt = stringHex

-- | Decode hexadecimal digits as 'Integer'.
--
--   >>> stringHexInteger "0F"
--   Just 15
--
stringHexInteger :: String -> Maybe Integer
stringHexInteger = stringHex


-- ----------------------  Encoder

integralLowerHexString :: (Integral n, Show n) => n -> String
integralLowerHexString n = Num.showHex n ""

integralUpperHexString :: (Integral n, Show n) => n -> String
integralUpperHexString = map Ch.toUpper . integralLowerHexString

-- | Encode integer to hexadecimal string.
--
--   >>> intHexString 15
--   "f"
--
intLowerHexString :: Int -> String
intLowerHexString = integralLowerHexString

-- | Encode integer to hexadecimal string.
--
--   >>> intUpperHexString 15
--   "F"
--
intUpperHexString :: Int -> String
intUpperHexString = integralUpperHexString
