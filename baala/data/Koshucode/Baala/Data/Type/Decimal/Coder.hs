{-# OPTIONS_GHC -Wall #-}

-- | Decode and encode of decimals.

module Koshucode.Baala.Data.Type.Decimal.Coder
  ( -- * Decode
    DecodeAb,
    decodeDecimal,  
  
    -- * Encode
    encodeDecimal,
    encodeDecimalCompact,
  ) where

import qualified Data.Char                                 as Ch
import qualified Data.Ratio                                as R
import qualified Koshucode.Baala.Base                      as B
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal as D
import qualified Koshucode.Baala.Data.Type.Message         as Msg


-- ----------------------  Decode

-- | Decode to @a@.
type DecodeAb a = String -> B.Ab a

type Sign = B.Map D.DecimalInteger

-- | Decode decimals.
decodeDecimal :: DecodeAb D.Decimal
decodeDecimal ccs = headPart id ccs where
    minus x = - x

    headPart _ [] = Msg.notNumber []
    headPart sign (c:cs) = case c of
        ' '  -> headPart sign  cs
        '-'  -> headPart minus cs
        '+'  -> headPart id    cs
        _    -> intPart sign 0 (c:cs)

    intPart :: Sign -> D.DecimalInteger -> DecodeAb D.Decimal
    intPart sign n [] = Right $ D.Decimal (sign n D.%% 1) 0 False
    intPart sign n (c:cs)
        | Ch.isDigit c = intPart  sign (10 * n + fromDigit c) cs
        | c == ' '     = intPart  sign n cs
        | c == '.'     = fracPart sign n 0 cs
        | otherwise    = tailPart False sign (n, 0) (c:cs)

    fracPart :: Sign -> D.DecimalInteger -> D.DecimalFracl -> DecodeAb D.Decimal
    fracPart sign n f [] = Right $ D.Decimal (sign n D.%% (10 ^ f)) f False
    fracPart sign n f (c:cs)
        | Ch.isDigit c = fracPart sign (10 * n + fromDigit c) (f + 1) cs
        | c == ' '     = fracPart sign n f cs
        | otherwise    = tailPart False sign (n, f) (c:cs)

    tailPart :: Bool -> Sign -> (D.DecimalInteger, D.DecimalFracl) -> DecodeAb D.Decimal
    tailPart approx sign (n, f) [] = Right $ D.Decimal (sign n D.%% (10 ^ f)) f approx
    tailPart approx sign dec (c:cs) = case c of
        ' '  -> tailPart approx sign  dec cs
        '-'  -> tailPart approx minus dec cs
        '+'  -> tailPart approx id    dec cs
        'a'  -> tailPart True   sign  dec cs
        'A'  -> tailPart True   sign  dec cs
        _    -> Msg.notNumber ccs

fromDigit :: Char -> D.DecimalInteger
fromDigit '0' = 0
fromDigit '1' = 1
fromDigit '2' = 2
fromDigit '3' = 3
fromDigit '4' = 4
fromDigit '5' = 5
fromDigit '6' = 6
fromDigit '7' = 7
fromDigit '8' = 8
fromDigit '9' = 9
fromDigit _   = B.bug "fromDigit"


-- ----------------------  Encode

-- | Encode decimals.
encodeDecimal :: D.Decimal -> String
encodeDecimal = encodeDecimalWith (' ' :)

-- | Encode decimals without spaces.
encodeDecimalCompact :: D.Decimal -> String
encodeDecimalCompact = encodeDecimalWith id

encodeDecimalWith :: B.Map String -> D.Decimal -> String
encodeDecimalWith g (D.Decimal r pt approx)
    | n >= 0     =       digits
    | otherwise  = '-' : digits
    where n     = R.numerator   r
          den   = R.denominator r
          digits = decimalDigits g approx pt (decimalShift pt n `div` den)

decimalShift :: D.DecimalFracl -> B.Map D.DecimalInteger
decimalShift pt n = (10 ^ abs pt) * (abs n)

decimalDigits :: B.Map String -> Bool -> D.DecimalFracl -> D.DecimalInteger -> String
decimalDigits g approx pt
    | pt == 0   = zero . reverse . intPart pt
    | otherwise = zero . reverse . fracPart pt
    where
    fracPart :: D.DecimalFracl -> D.DecimalInteger -> String
    fracPart 0 n = '.' : intPart 0 n
    fracPart p n = case quotDigit n of
                    (n', d) -> d : fracPart (p - 1) n'

    intPart :: D.DecimalFracl -> D.DecimalInteger -> String
    intPart _ 0 | approx      = " a"
                | otherwise   = ""
    intPart 3 n               = g $ intPart 0 n
    intPart p n               = case quotDigit n of
                                  (n', d) -> d : intPart (p + 1) n'

    zero ""           = "0"
    zero ds@('.' : _) = '0' : ds
    zero ds           = ds

quotDigit :: D.DecimalInteger -> (D.DecimalInteger, Char)
quotDigit n = case quotRem n 10 of
                (n', d) -> (n', Ch.chr $ fromInteger d + Ch.ord '0')

-- map quotDigit [0..9]
-- map quotDigit [10..19]
-- map quotDigit [100..109]

