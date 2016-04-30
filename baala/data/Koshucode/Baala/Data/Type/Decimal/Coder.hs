{-# OPTIONS_GHC -Wall #-}

-- | Decode and encode of decimals.
--
--   [Decimal]
--     Head Int Frac ? Tail
--   [Head]
--     Sign ...
--   [Int]
--     Digit ...
--   [Frac]
--     Separator Digit ...
--   [Tail]
--     Sign ...
--   [Sign]
--     Space | @"-"@ | @"+"@
--   [Digit]
--     Space | @"0"@ | ... | @"9"@ | @"a"@ | ... | @"z"@ | @\"A\"@ | ... | @\"Z\"@
--   [Separator]
--     @"."@
--   [Space]
--     @" "@

module Koshucode.Baala.Data.Type.Decimal.Coder
  ( -- * Decode
    DecodeAb,
    decodeBinary,
    decodeOctal,
    decodeDecimal,
    decodeHex,
    decodeBase,
  
    -- * Encode
    encodeDecimal,
    encodeDecimalCompact,
  ) where

import qualified Data.Char                                   as Ch
import qualified Data.Ratio                                  as R
import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal   as D
import qualified Koshucode.Baala.Data.Type.Decimal.Rational  as D
import qualified Koshucode.Baala.Data.Type.Message           as Msg


-- ----------------------  Decode

-- | Decode to @a@.
type DecodeAb a = String -> B.Ab a

type Sign = B.Map D.DecimalInteger

-- | Decode base-2 digits to decimal.
decodeBinary :: DecodeAb D.Decimal
decodeBinary = decodeBase 2

-- | Decode base-8 digits to decimal.
decodeOctal :: DecodeAb D.Decimal
decodeOctal = decodeBase 8

-- | Decode decimals.
--
-- >>> decodeDecimal "11"
-- Right (Decimal { ratio = 11 % 1, fracl = 0 })
--
-- >>> decodeDecimal "-12 345.00"
-- Right (Decimal { ratio = -12345 % 1, fracl = 2 })
--
-- >>> decodeDecimal "11.250 +"
-- Right (Decimal { ratio = 45 % 4, fracl = 3 })

decodeDecimal :: DecodeAb D.Decimal
decodeDecimal = decodeBase 10

-- | Decode base-16 digits to decimal.
decodeHex :: DecodeAb D.Decimal
decodeHex = decodeBase 16

-- | Decode digits to number.
decodeBase :: Integer -> DecodeAb D.Decimal
decodeBase base ccs = headPart id ccs where
    minus x = - x

    headPart _ [] = Msg.notNumber []
    headPart sign (c:cs) = case c of
        ' '  -> headPart sign  cs
        '-'  -> headPart minus cs
        '+'  -> headPart sign  cs
        _    -> intPart  sign  0 (c:cs)

    intPart :: Sign -> D.DecimalInteger -> DecodeAb D.Decimal
    intPart sign n [] = decimal 0 (sign n) False
    intPart sign n (c:cs)
        = case digitToInteger c of
            Just i           -> do n' <- up c i n
                                   intPart  sign n' cs
            Nothing | c == ' '  -> intPart  sign n cs
                    | c == '.'  -> fracPart sign n 0 cs
                    | otherwise -> tailPart False sign (n, 0) (c:cs)

    fracPart :: Sign -> D.DecimalInteger -> D.DecimalFracl -> DecodeAb D.Decimal
    fracPart sign n f [] = decimal f (sign n) False
    fracPart sign n f (c:cs)
        = case digitToInteger c of
            Just i           -> do n' <- up c i n
                                   fracPart sign n' (f + 1) cs
            Nothing | c == ' '  -> fracPart sign n f cs
                    | otherwise -> tailPart False sign (n, f) (c:cs)

    tailPart :: Bool -> Sign -> (D.DecimalInteger, D.DecimalFracl) -> DecodeAb D.Decimal
    tailPart approx sign (n, f) [] = decimal f (sign n) approx
    tailPart approx sign dec (c:cs) = case c of
        ' '  -> tailPart approx sign  dec cs
        '-'  -> tailPart approx minus dec cs
        '+'  -> tailPart approx id    dec cs
        'a'  -> tailPart True   sign  dec cs
        'A'  -> tailPart True   sign  dec cs
        _    -> Msg.notNumber ccs

    up c i n | i < base   = Right $ base * n + i
             | otherwise  = Msg.tooLargeDigit [c]

    decimal f n approx = Right $ D.Decimal { D.decimalRatio  = r f
                                           , D.decimalFracl  = f
                                           , D.decimalApprox = approx }
        where r 0 = n D.%% 1
              r 1 = n D.%% 10
              r 2 = n D.%% 100
              r _ = n D.%% (base ^ f)

digitToInteger :: Char -> Maybe Integer
digitToInteger c
    | between '0' '9'  = Just $ i - ord '0'
    | between 'a' 'z'  = Just $ i - ord 'a' + 10
    | between 'A' 'Z'  = Just $ i - ord 'A' + 10
    | otherwise        = Nothing
    where between a z = c >= a && c <= z
          i = ord c

ord :: Char -> Integer
ord = fromIntegral. Ch.ord


-- ----------------------  Encode

-- | Encode decimals.
encodeDecimal :: D.Decimal -> String
encodeDecimal = encodeDecimalWith (' ' :)

-- | Encode decimals without spaces.
encodeDecimalCompact :: D.Decimal -> String
encodeDecimalCompact = encodeDecimalWith id

encodeDecimalWith :: B.Map String -> D.Decimal -> String
encodeDecimalWith g D.Decimal { D.decimalRatio = r, D.decimalFracl = pt, D.decimalApprox = approx }
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

