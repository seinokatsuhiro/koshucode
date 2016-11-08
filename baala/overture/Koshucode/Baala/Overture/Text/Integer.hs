{-# OPTIONS_GHC -Wall #-}

-- | Encode and decode integers.

module Koshucode.Baala.Overture.Text.Integer
  ( -- * Decoder
    stringDec, stringInt, stringInteger,
    stringHex, stringHexInt, stringHexInteger,
    -- * Encoder
    intLowerHexString, intUpperHexString,
  ) where

import qualified Data.Char                      as Ch
import qualified Numeric                        as Num


-- ----------------------  Decoder

stringMaybe :: (String -> [(a, String)]) -> String -> Maybe a
stringMaybe f s = case f s of
                  [(x, "")] -> Just x
                  _         -> Nothing

-- | Decode decimal integer.
stringDec :: (Eq n, Num n) => String -> Maybe n
stringDec = stringMaybe Num.readDec

-- | Decode decimal integer as 'Int'.
--
--   >>> stringInt "12"
--   Just 12
--
--   >>> stringInt "12.3"
--   Nothing
--
--   >>> stringInt "12345678901234567890"
--   Just (-6101065172474983726)
--
stringInt :: String -> Maybe Int
stringInt = stringDec

-- | Decode decimal integer as 'Integer'.
--
--   >>> stringInteger "12345678901234567890"
--   Just 12345678901234567890
--
stringInteger :: String -> Maybe Integer
stringInteger = stringDec

-- | Decode hexadecimal digits.
stringHex :: (Eq n, Num n) => String -> Maybe n
stringHex = stringMaybe Num.readHex

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
