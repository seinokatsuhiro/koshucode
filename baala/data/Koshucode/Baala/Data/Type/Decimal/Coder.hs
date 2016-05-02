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
    intPart sign n [] = decimal 0 (sign n)
    intPart sign n (c:cs)
        = case digitToInteger c of
            Just i           -> do n' <- up c i n
                                   intPart  sign n' cs
            Nothing | c == ' '  -> intPart  sign n cs
                    | c == 'o'  -> ooPart   sign (-1) n cs
                    | c == '.'  -> fracPart sign n 0 cs
                    | otherwise -> tailPart sign (n, 0) (c:cs)

    ooPart :: Sign -> D.DecimalFracl -> D.DecimalInteger-> DecodeAb D.Decimal
    ooPart sign l n [] = decimal l (sign n)
    ooPart sign l n (c:cs)
        | c == ' '   = ooPart   sign l n cs
        | c == 'o'   = ooPart   sign (l - 1) n cs
        | otherwise  = tailPart sign (n, l) (c:cs)

    fracPart :: Sign -> D.DecimalInteger -> D.DecimalFracl -> DecodeAb D.Decimal
    fracPart sign n f [] = decimal f (sign n)
    fracPart sign n f (c:cs)
        = case digitToInteger c of
            Just i           -> do n' <- up c i n
                                   fracPart sign n' (f + 1) cs
            Nothing | c == ' '  -> fracPart sign n f cs
                    | otherwise -> tailPart sign (n, f) (c:cs)

    tailPart :: Sign -> (D.DecimalInteger, D.DecimalFracl) -> DecodeAb D.Decimal
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

    decimal l n = Right $ D.Decimal { D.decimalFracl  = l
                                    , D.decimalRatio  = r l }
        where n'  = n D.%% 1
              r 0 = n'
              r _ = n' * D.ratioFracl l

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

separator :: B.Map String
separator ""            = ""
separator [c]           = [c]
separator cs@(' ' : _)  = cs
separator cs            = ' ' : cs

-- | Encode decimals.
--
-- >>> encodeDecimal $ D.realDecimal 0 (12345 D.%% 10)
-- "1 234"
--
-- >>> encodeDecimal $ D.realDecimal 2 (12345 D.%% 10)
-- "1 234.50"
--
-- >>> encodeDecimal $ D.realDecimal (-2) (12345 D.%% 10)
-- "1 2oo"

encodeDecimal :: D.Decimal -> String
encodeDecimal = encodeDecimalWith separator

-- | Encode decimals without spaces.
encodeDecimalCompact :: D.Decimal -> String
encodeDecimalCompact = encodeDecimalWith id

encodeDecimalWith :: B.Map String -> D.Decimal -> String
encodeDecimalWith sep D.Decimal { D.decimalFracl = l, D.decimalRatio = r } =
    decimalSign r $ case ratioDigits sep l $ abs r of
                      (int, frac) | l > 0      -> int ++ "." ++ frac
                                  | otherwise  -> int

decimalSign :: D.DecimalRatio -> B.Map String
decimalSign r cs | r < 0      = '-' : cs
                 | otherwise  = cs

ratioDigits :: (Integral n) => B.Map String -> D.DecimalFracl -> R.Ratio n -> (String, String)
ratioDigits sep l r = case properFraction r of
                        (i, r') -> (integerDigits sep l i, fracDigits sep l r')

integerDigits :: B.Map String -> D.DecimalFracl -> Integer -> String
integerDigits sep = loop 0 "" where
    loop :: Int -> String -> D.DecimalFracl -> Integer -> String
    loop n cs l i
        | i == 0  = if null cs then "0" else cs
        | n == 3  = loop 0 (sep cs) l i
        | i > 0   = case i `quotRem` 10 of
                      (q, r) -> loop (n + 1) (up l r : cs) (l + 1) q
        | otherwise = cs

    up l r | l < 0      = 'o'
           | otherwise  = Ch.intToDigit (fromInteger r)

fracDigits :: (Integral n) => B.Map String -> D.DecimalFracl -> R.Ratio n -> String
fracDigits sep = loop (0 :: Int) where
    loop n l 0 = fill n l
    loop _ 0 _ = ""
    loop 3 l r = sep $ loop 0 l r
    loop n l r = case properFraction $ 10 * r of
                   (i, r') -> Ch.intToDigit i : loop (n + 1) (l - 1) r'

    fill 3 l              = sep $ fill 0 l
    fill n l | l > 0      = '0' : fill (n + 1) (l - 1)
             | otherwise  = ""

