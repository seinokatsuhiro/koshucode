{-# OPTIONS_GHC -Wall #-}

-- | Encode and decode integers.

module Koshucode.Baala.Overture.Text.Integer
  ( -- * Decoder
    stringDec, stringInt, stringInteger,
    stringHex, stringHexInt, stringHexInteger,
    stringCustomInteger, stringCountInteger,

    -- * Encoder
    intLowerHexString, intUpperHexString,
    integralCustomString, integralCountString,
    digitsLength,
  ) where

import qualified Data.Char                      as Ch
import qualified Data.IntMap.Strict             as Mi
import qualified Data.Map.Strict                as Ms
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

-- | Decode custom digits to integer.
--
--   >>> stringCustomInteger "01234567" "144"
--   Just 100
--
stringCustomInteger :: String -> String -> Maybe Integer
stringCustomInteger = stringIntegerFrom 0

-- | Decode counting digits to integer.
--
--   >>> stringCountInteger ['A'..'Z'] <$> ["", "A", "B", "Y", "Z", "AA", "AZ", "BA", "CV", "ALL", "KOSHU"]
--   [Just 0, Just 1, Just 2, Just 25, Just 26, Just 27, Just 52, Just 53, Just 100, Just 1000, Just 5303449]
--
stringCountInteger :: String -> String -> Maybe Integer
stringCountInteger = stringIntegerFrom 1

stringIntegerFrom :: Integer -> String -> String -> Maybe Integer
stringIntegerFrom from digits = loop 0 where
    m = Ms.fromList $ zip digits [from ..]
    b = toInteger $ length digits

    loop n "" = Just n
    loop n (d:ds) = case Ms.lookup d m of
                      Nothing -> Nothing
                      Just i  -> loop (b * n + i) ds


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
    dig  = Mi.fromList $ zip [0..] digits
    base = integralLength digits
    loop s 0 | null s    = [dig Mi.! 0]
             | otherwise = s
    loop s n = case quotRem n base of
                 (n', i) -> loop (dig ! i : s) n'

-- | Encode integer to counting string.
--
--   >>> intReckonString ['A'..'Z'] <$> [0, 1, 2, 25, 26, 27, 52, 53, 100, 1000, 5303449]
--   ["", "A", "B", "Y", "Z", "AA", "AZ", "BA", "CV", "ALL", "KOSHU"]
--
integralCountString :: (Integral n) => String -> n -> String
integralCountString digits = loop "" . abs where
    dig       = Mi.fromList $ zip [0..] $ '_' : digits
    base      = integralLength digits
    loop s 0  = s
    loop s n  = case countDiv n base of
                  (n', m) -> loop (dig ! m : s) n'

-- | Integral version
(!) :: (Integral n) => Mi.IntMap a -> n -> a
(!) m n = m Mi.! fromIntegral n

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
--  >>> (`countDiv` 3) <$> [1 .. 10 :: Int]
--  [(0,1),(0,2),(0,3),(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1)]
--
countDiv :: (Integral n) => n -> n -> (n, n)
countDiv x y =
    case quotRem x y of
      (q, r) | r == 0    -> (q - 1, y)
             | otherwise -> (q, r)

-- | Length of digits of integer.
--
--   >>> digitsLength 10 12000
--   5
--
--   >>> digitsLength 10 (-500)
--   4
--
--   >>> digitsLength 2 15
--   4
--
digitsLength :: Int -> Int -> Int
digitsLength b n0
    | n0 >  0   = loop n0
    | n0 <  0   = 1 + loop (abs n0)
    | otherwise = 1
    where
      loop 0 = 0
      loop n = 1 + loop (n `div` b)

