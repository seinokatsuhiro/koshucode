{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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
type DecodeAb t a = t -> B.Ab a

type Sign = O.Map T.DecimalInteger

-- | Decode base-2 digits to decimal.
decodeBinary :: (O.Textual t) => DecodeAb t T.Decimal
decodeBinary = decodeBase 2

-- | Decode base-8 digits to decimal.
decodeOctal :: (O.Textual t) => DecodeAb t T.Decimal
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

decodeDecimal ::(O.Textual t) => DecodeAb t T.Decimal
decodeDecimal = decodeBase 10

-- | Decode base-16 digits to decimal.
decodeHex :: (O.Textual t) => DecodeAb t T.Decimal
decodeHex = decodeBase 16

-- | Decode digits to number.
decodeBase :: forall t. (O.Textual t) => Integer -> DecodeAb t T.Decimal
decodeBase base ccs = headPart id ccs where
    minus x = - x

    headPart sign (O.tCut -> O.Jp c cs) = case c of
        ' '  -> headPart sign  cs
        '-'  -> headPart minus cs
        '+'  -> headPart sign  cs
        _    -> intPart  sign  0 (c O.<:> cs)
    headPart _ _ = Msg.notNumber ccs

    intPart :: Sign -> T.DecimalInteger -> DecodeAb t T.Decimal
    intPart sign n (O.tCut -> O.Jp c cs)
        = case digitToInteger c of
            Just i           -> do n' <- up c i n
                                   intPart  sign n' cs
            Nothing | c == ' '  -> intPart  sign n cs
                    | c == 'o'  -> ooPart   sign (-1) n cs
                    | c == '.'  -> fracPart sign n 0 cs
                    | otherwise -> tailPart sign (n, 0) (c O.<:> cs)
    intPart sign n _ = decimal 0 (sign n)

    ooPart :: Sign -> T.DecimalFracle -> T.DecimalInteger-> DecodeAb t T.Decimal
    ooPart sign l n (O.tCut -> O.Jp c cs)
        | c == ' '   = ooPart   sign l n cs
        | c == 'o'   = ooPart   sign (l - 1) n cs
        | otherwise  = tailPart sign (n, l) (c O.<:> cs)
    ooPart sign l n _ = decimal l (sign n)

    fracPart :: Sign -> T.DecimalInteger -> T.DecimalFracle -> DecodeAb t T.Decimal
    fracPart sign n f (O.tCut -> O.Jp c cs)
        = case digitToInteger c of
            Just i           -> do n' <- up c i n
                                   fracPart sign n' (f + 1) cs
            Nothing | c == ' '  -> fracPart sign n f cs
                    | otherwise -> tailPart sign (n, f) (c O.<:> cs)
    fracPart sign n f _ = decimal f (sign n)

    tailPart :: Sign -> (T.DecimalInteger, T.DecimalFracle) -> DecodeAb t T.Decimal
    tailPart sign dec (O.tCut -> O.Jp c cs) = case c of
        ' '  -> tailPart sign  dec cs
        '-'  -> tailPart minus dec cs
        '+'  -> tailPart id    dec cs
        'a'  -> tailPart sign  dec cs
        'A'  -> tailPart sign  dec cs
        _    -> Msg.notNumber ccs
    tailPart sign (n, f) _ = decimal f (sign n)

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

-- | Digit separator.
--
--   >>> separator "1" :: String
--   "1"
--
--   >>> separator "1234" :: String
--   " 1234"
--
separator :: (O.Textual t) => O.Map t
separator t@(O.tCut2 -> Just (_, Nothing))  = t
separator t@(O.tCut  -> O.Jp ' ' _)         = t
separator t | O.tIsEmpty t                  = t
            | otherwise                     = ' ' O.<:> t

-- | Encode decimals.
--
-- >>> encodeDecimal $ T.realDecimal 0 (12345 T.%% 10) :: String
-- "1 235"
--
-- >>> encodeDecimal $ T.realDecimal 2 (12345 T.%% 10) :: String
-- "1 234.50"
--
-- >>> encodeDecimal $ T.realDecimal (-2) (12345 T.%% 10) :: String
-- "1 3oo" :: String
--
encodeDecimal :: (O.Textual t) => T.Decimal -> t
encodeDecimal = encodeDecimalWith separator

-- | Encode decimals without spaces.
encodeDecimalCompact :: (O.Textual t) => T.Decimal -> t
encodeDecimalCompact = encodeDecimalWith id

encodeDecimalWith :: (O.Textual t) => O.Map t -> T.Decimal -> t
encodeDecimalWith sep = encode . T.decimalRoundOut where
    encode T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
        decimalSign r $ case ratioDigits sep l $ abs r of
                          (int, frac) | l > 0      -> int O.++ ('.' O.<:> frac)
                                      | otherwise  -> int

decimalSign :: (O.Textual t) => T.DecimalRatio -> O.Map t
decimalSign r cs | r < 0      = '-' O.<:> cs
                 | otherwise  = cs

ratioDigits :: (O.Textual t, Integral n) => O.Map t -> T.DecimalFracle -> R.Ratio n -> (t, t)
ratioDigits sep l r = case properFraction r of
                        (i, r') -> (integerDigits sep l i, fracDigits sep l r')

integerDigits :: forall t. (O.Textual t) => O.Map t -> T.DecimalFracle -> Integer -> t
integerDigits sep = loop 0 O.tEmpty where
    loop :: Int -> t -> T.DecimalFracle -> Integer -> t
    loop n cs l i
        | i == 0  = if O.tIsEmpty cs then O.charT '0' else cs
        | n == 3  = loop 0 (sep cs) l i
        | i > 0   = case i `quotRem` 10 of
                      (q, r) -> loop (n + 1) (up l r O.<:> cs) (l + 1) q
        | otherwise = cs

    up l r | l < 0      = 'o'
           | otherwise  = Ch.intToDigit (fromInteger r)

fracDigits :: (O.Textual t, Integral n) => O.Map t -> T.DecimalFracle -> R.Ratio n -> t
fracDigits sep = loop (0 :: Int) where
    loop n l 0 = fill n l
    loop _ 0 _ = O.tEmpty
    loop 3 l r = sep $ loop 0 l r
    loop n l r = case properFraction $ 10 * r of
                   (i, r') -> Ch.intToDigit i O.<:> loop (n + 1) (l - 1) r'

    fill 3 l              = sep $ fill 0 l
    fill n l | l > 0      = '0' O.<:> fill (n + 1) (l - 1)
             | otherwise  = O.tEmpty

