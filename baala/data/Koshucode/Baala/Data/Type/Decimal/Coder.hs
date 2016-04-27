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
import qualified Koshucode.Baala.Base                      as B
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal as D
import qualified Koshucode.Baala.Data.Type.Message         as Msg


-- ----------------------  Decode

-- | Decode to @a@.
type DecodeAb a = String -> B.Ab a

-- | Decode decimals.
decodeDecimal :: DecodeAb D.Decimal
decodeDecimal ccs = headPart id ccs where
    minus x = - x

    headPart _ [] = Msg.notNumber []
    headPart sign (c:cs) = case c of
        ' '  ->  headPart sign  cs
        '-'  ->  headPart minus cs
        '+'  ->  headPart id    cs
        _    ->  intPart sign 0 (c:cs)

    intPart :: B.Map D.DecimalInteger -> D.DecimalInteger -> DecodeAb D.Decimal
    intPart sign n [] = Right $ D.Decimal (sign n, 1) 0 False
    intPart sign n (c:cs)
        | Ch.isDigit c =  intPart sign (10 * n + fromDigit c) cs
        | c == ' '     =  intPart sign n cs
        | c == '.'     =  decPart sign n 0 cs
        | otherwise    =  tailPart False sign (n, 0) (c:cs)

    decPart :: B.Map D.DecimalInteger -> D.DecimalInteger -> D.DecimalPoint -> DecodeAb D.Decimal
    decPart sign n p [] = Right $ D.Decimal (sign n, 10 ^ p) p False
    decPart sign n p (c:cs)
        | Ch.isDigit c = decPart sign (10 * n + fromDigit c) (p + 1) cs
        | c == ' '     =  decPart sign n p cs
        | otherwise    =  tailPart False sign (n, p) (c:cs)

    tailPart :: Bool -> B.Map D.DecimalInteger -> (D.DecimalInteger, D.DecimalPoint) -> DecodeAb D.Decimal
    tailPart approx sign (n, p) [] = Right $ D.Decimal (sign n, 10 ^ p) p approx
    tailPart approx sign dec (c:cs) = case c of
        ' '  ->  tailPart approx sign  dec cs
        '-'  ->  tailPart approx minus dec cs
        '+'  ->  tailPart approx id    dec cs
        'a'  ->  tailPart True   sign  dec cs
        'A'  ->  tailPart True   sign  dec cs
        _    ->  Msg.notNumber ccs

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
encodeDecimalWith g (D.Decimal (n, den) pt approx)
    | n >= 0     =       digits
    | otherwise  = '-' : digits
    where digits = decimalDigits g approx pt (decimalShift pt n `div` den)

decimalShift :: D.DecimalPoint -> B.Map D.DecimalInteger
decimalShift pt n = (10 ^ abs pt) * (abs n)

decimalDigits :: B.Map String -> Bool -> D.DecimalPoint -> D.DecimalInteger -> String
decimalDigits g approx pt
    | pt == 0   = zero . reverse . intPart pt
    | otherwise = zero . reverse . decPart pt
    where
    decPart :: D.DecimalPoint -> D.DecimalInteger -> String
    decPart 0 n = '.' : intPart 0 n
    decPart p n = case quotDigit n of
                    (n', d) -> d : decPart (p - 1) n'

    intPart :: D.DecimalPoint -> D.DecimalInteger -> String
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

