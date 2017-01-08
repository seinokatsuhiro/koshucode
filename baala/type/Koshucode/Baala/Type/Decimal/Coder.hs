{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

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

module Koshucode.Baala.Type.Decimal.Coder
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

import qualified Data.Char                                  as Ch
import qualified Data.Ratio                                 as R
import qualified Koshucode.Baala.Overture                   as O
import qualified Koshucode.Baala.Base                       as B
import qualified Koshucode.Baala.Type.Decimal.Decimal       as T
import qualified Koshucode.Baala.Type.Decimal.Fraction      as T
import qualified Koshucode.Baala.Type.Decimal.Rational      as T
import qualified Koshucode.Baala.Type.Message               as Msg


-- ----------------------  Decode

-- | Decode to @a@.
type DecodeAb a = String -> B.Ab a

type Sign = O.Map T.DecimalInteger

-- | Decode base-2 digits to decimal.
decodeBinary :: DecodeAb T.Decimal
decodeBinary = decodeBase 2

-- | Decode base-8 digits to decimal.
decodeOctal :: DecodeAb T.Decimal
decodeOctal = decodeBase 8

-- | Decode decimals.
--
-- >>> decodeDecimal "11"
-- Right Decimal (0) 11
--
-- >>> decodeDecimal "-12 345.00"
-- Right Decimal (2) -12345
--
-- >>> decodeDecimal "11.250 +"
-- Right Decimal (3) 11 + 1 % 4

decodeDecimal :: DecodeAb T.Decimal
decodeDecimal = decodeBase 10

-- | Decode base-16 digits to decimal.
decodeHex :: DecodeAb T.Decimal
decodeHex = decodeBase 16

-- | Decode digits to number.
decodeBase :: Integer -> DecodeAb T.Decimal
decodeBase base ccs = headPart id ccs where
    minus x = - x

    headPart _ [] = Msg.notNumber []
    headPart sign (c:cs) = case c of
        ' '  -> headPart sign  cs
        '-'  -> headPart minus cs
        '+'  -> headPart sign  cs
        _    -> intPart  sign  0 (c:cs)

    intPart :: Sign -> T.DecimalInteger -> DecodeAb T.Decimal
    intPart sign n [] = decimal 0 (sign n)
    intPart sign n (c:cs)
        = case digitToInteger c of
            Just i           -> do n' <- up c i n
                                   intPart  sign n' cs
            Nothing | c == ' '  -> intPart  sign n cs
                    | c == 'o'  -> ooPart   sign (-1) n cs
                    | c == '.'  -> fracPart sign n 0 cs
                    | otherwise -> tailPart sign (n, 0) (c:cs)

    ooPart :: Sign -> T.DecimalFracle -> T.DecimalInteger-> DecodeAb T.Decimal
    ooPart sign l n [] = decimal l (sign n)
    ooPart sign l n (c:cs)
        | c == ' '   = ooPart   sign l n cs
        | c == 'o'   = ooPart   sign (l - 1) n cs
        | otherwise  = tailPart sign (n, l) (c:cs)

    fracPart :: Sign -> T.DecimalInteger -> T.DecimalFracle -> DecodeAb T.Decimal
    fracPart sign n f [] = decimal f (sign n)
    fracPart sign n f (c:cs)
        = case digitToInteger c of
            Just i           -> do n' <- up c i n
                                   fracPart sign n' (f + 1) cs
            Nothing | c == ' '  -> fracPart sign n f cs
                    | otherwise -> tailPart sign (n, f) (c:cs)

    tailPart :: Sign -> (T.DecimalInteger, T.DecimalFracle) -> DecodeAb T.Decimal
    tailPart sign (n, f) [] = decimal f (sign n)
    tailPart sign dec (c:cs) = case c of
        ' '  -> tailPart sign  dec cs
        '-'  -> tailPart minus dec cs
        '+'  -> tailPart id    dec cs
        'a'  -> tailPart sign  dec cs
        'A'  -> tailPart sign  dec cs
        _    -> Msg.notNumber ccs

    up c i n | i < base   = Right $ base * n + i
             | otherwise  = Msg.tooLargeDigit [c]

    decimal l n = Right $ T.Decimal { T.decimalFracle = l
                                    , T.decimalRatio  = r l }
        where n'  = n T.%% 1
              r 0 = n'
              r _ = n' * T.ratioFracle l

digitToInteger :: Char -> Maybe Integer
digitToInteger c
    | between '0' '9'  = Just $ i - ord '0'
    | between 'a' 'f'  = Just $ i - ord 'a' + 10
    | between 'A' 'F'  = Just $ i - ord 'A' + 10
    | otherwise        = Nothing
    where between a z = c >= a && c <= z
          i = ord c

ord :: Char -> Integer
ord = fromIntegral. Ch.ord


-- ----------------------  Encode

instance B.MixEncode T.Decimal where
    mixTransEncode _ dec = B.mixString $ encodeDecimal dec

separator :: O.StringMap
separator ""            = ""
separator [c]           = [c]
separator cs@(' ' : _)  = cs
separator cs            = ' ' : cs

-- | Encode decimals.
--
-- >>> encodeDecimal $ T.realDecimal 0 (12345 T.%% 10)
-- "1 234"
--
-- >>> encodeDecimal $ T.realDecimal 2 (12345 T.%% 10)
-- "1 234.50"
--
-- >>> encodeDecimal $ T.realDecimal (-2) (12345 T.%% 10)
-- "1 2oo"

encodeDecimal :: T.Decimal -> String
encodeDecimal = encodeDecimalWith separator

-- | Encode decimals without spaces.
encodeDecimalCompact :: T.Decimal -> String
encodeDecimalCompact = encodeDecimalWith id

encodeDecimalWith :: O.StringMap -> T.Decimal -> String
encodeDecimalWith sep = encode . T.decimalRoundOut where
    encode T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
        decimalSign r $ case ratioDigits sep l $ abs r of
                          (int, frac) | l > 0      -> int ++ "." ++ frac
                                      | otherwise  -> int

decimalSign :: T.DecimalRatio -> O.StringMap
decimalSign r cs | r < 0      = '-' : cs
                 | otherwise  = cs

ratioDigits :: (Integral n) => O.StringMap -> T.DecimalFracle -> R.Ratio n -> (String, String)
ratioDigits sep l r = case properFraction r of
                        (i, r') -> (integerDigits sep l i, fracDigits sep l r')

integerDigits :: O.StringMap -> T.DecimalFracle -> Integer -> String
integerDigits sep = loop 0 "" where
    loop :: Int -> String -> T.DecimalFracle -> Integer -> String
    loop n cs l i
        | i == 0  = if null cs then "0" else cs
        | n == 3  = loop 0 (sep cs) l i
        | i > 0   = case i `quotRem` 10 of
                      (q, r) -> loop (n + 1) (up l r : cs) (l + 1) q
        | otherwise = cs

    up l r | l < 0      = 'o'
           | otherwise  = Ch.intToDigit (fromInteger r)

fracDigits :: (Integral n) => O.StringMap -> T.DecimalFracle -> R.Ratio n -> String
fracDigits sep = loop (0 :: Int) where
    loop n l 0 = fill n l
    loop _ 0 _ = ""
    loop 3 l r = sep $ loop 0 l r
    loop n l r = case properFraction $ 10 * r of
                   (i, r') -> Ch.intToDigit i : loop (n + 1) (l - 1) r'

    fill 3 l              = sep $ fill 0 l
    fill n l | l > 0      = '0' : fill (n + 1) (l - 1)
             | otherwise  = ""

