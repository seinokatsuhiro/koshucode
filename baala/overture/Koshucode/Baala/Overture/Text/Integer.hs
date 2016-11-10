{-# OPTIONS_GHC -Wall #-}

-- | Encode and decode integers.

module Koshucode.Baala.Overture.Text.Integer
  ( -- * Decoder
    stringDec, stringInt, stringInteger,
    stringHex, stringHexInt, stringHexInteger,
    -- * Encoder
    intLowerHexString, intUpperHexString,
    integralCustomString, integralReckonString,
  ) where

import qualified Data.Char                      as Ch
import qualified Data.IntMap.Strict             as Map
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

-- | Encode integer to custome-digit string.
--
--   >>> integralCustomString "OI" <$> [0..8 :: Int]
--   ["O", "I", "IO", "II", "IOO", "IOI", "IIO", "III", "IOOO"]
--
integralCustomString :: (Integral n) => String -> n -> String
integralCustomString digits = loop "" . abs where
    dig  = Map.fromList $ zip [0..] digits
    base = integralLength digits
    loop s 0 | null s    = [dig Map.! 0]
             | otherwise = s
    loop s n = case quotRem n base of
                 (n', i) -> loop (dig ! i : s) n'

-- | Encode integer to reckoning string.
--
--   >>> intReckonString ['A'..'Z'] <$> [0, 1, 2, 25, 26, 27, 52, 53, 100, 1000, 5303449]
--   ["", "A", "B", "Y", "Z", "AA", "AZ", "BA", "CV", "ALL", "KOSHU"]
--
integralReckonString :: (Integral n) => String -> n -> String
integralReckonString digits = loop "" . abs where
    dig       = Map.fromList $ zip [0..] $ '_' : digits
    base      = integralLength digits
    loop s 0  = s
    loop s n  = case reckonDiv n base of
                  (n', m) -> loop (dig ! m : s) n'

-- | Integral version
(!) :: (Integral n) => Map.IntMap a -> n -> a
(!) m n = m Map.! fromIntegral n

-- | Integral version of 'length'.
--
--   >>> integralLength "abc" :: Integer
--   3
--
integralLength :: (Integral n) => [a] -> n
integralLength [] = 0
integralLength (_ : xs) = 1 + integralLength xs

-- | Reckoning division.
--
--  >>> (`reckonDiv` 3) <$> [1 .. 10 :: Int]
--  [(0,1),(0,2),(0,3),(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1)]
--
reckonDiv :: (Integral n) => n -> n -> (n, n)
reckonDiv x y =
    case quotRem x y of
      (q, r) | r == 0    -> (q - 1, y)
             | otherwise -> (q, r)
