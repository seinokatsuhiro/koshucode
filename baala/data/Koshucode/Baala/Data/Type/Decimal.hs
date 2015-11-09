{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Decimal
  ( -- * Type
    Decimal (..),
    DecimalInteger,
    isDecimalZero,
    decimalNum, decimalDenom,
    decimalPointSet,
    reduceDecimal,

    -- * Convert
    integralDecimal, realDecimal,
    decimalFractional,
    decimal0, decimal1,
  
    -- * Reader
    LitString,
    LitDecimal,
    litDecimal,  
  
    -- * Writer
    decimalString,
  ) where

import qualified Data.Char                         as Ch
import qualified Data.Ratio                        as R
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Type.Message as Msg



-- ----------------------  Type

type DecimalInteger = Integer

data Decimal = Decimal 
    { decimalRatio   :: (DecimalInteger, DecimalInteger)
    , decimalPoint   :: Int
    , decimalApprox  :: Bool
    } deriving (Show, Eq, Ord)

-- | Test decimal is zero.
isDecimalZero :: Decimal -> Bool
isDecimalZero (Decimal (n, _) _ _)  = n == 0

-- | Numerator part of decimal number.
decimalNum :: Decimal -> DecimalInteger
decimalNum = fst . decimalRatio

-- | Denominator part of decimal number.
decimalDenom :: Decimal -> DecimalInteger
decimalDenom = snd . decimalRatio

-- | Change decimal point.
decimalPointSet :: Int -> B.AbMap Decimal
decimalPointSet p (Decimal r _ a) = Right $ Decimal r p a

reduceDecimal :: (DecimalInteger, DecimalInteger) -> Int -> Bool -> Decimal
reduceDecimal (n, den) = Decimal (n `div` g, den `div` g) where
    g = gcd n den


-- ----------------------  Convert

-- | Convert integral number to decimal number.
integralDecimal :: (Integral n) => n -> Decimal
integralDecimal = realDecimal 0

-- | Convert real number to decimal number.
realDecimal :: (Real n) => Int -> n -> Decimal
realDecimal p n = Decimal (R.numerator r, R.denominator r) p False where
    r = toRational n

-- | Convert decimal number to fractional number.
decimalFractional :: (Fractional n) => Decimal -> n
decimalFractional (Decimal { decimalRatio = (num, den)}) =
    fromRational $ toRational $ (num R.% den)

decimal0 :: Decimal
decimal0 = integralDecimal (0 :: DecimalInteger)

decimal1 :: Decimal
decimal1 = integralDecimal (1 :: DecimalInteger)


-- ----------------------  Reader

{-| Make @a@ from a string. -}
type LitString a = String -> B.Ab a

type LitDecimal = LitString Decimal

litDecimal :: LitDecimal
litDecimal ccs = headPart id ccs where
    minus x = - x

    headPart _ [] = Msg.notNumber []
    headPart sign (c:cs) = case c of
        ' '  ->  headPart sign  cs
        '-'  ->  headPart minus cs
        '+'  ->  headPart id    cs
        _    ->  intPart sign 0 (c:cs)

    intPart :: B.Map DecimalInteger -> DecimalInteger -> LitDecimal
    intPart sign n [] = Right $ Decimal (sign n, 1) 0 False
    intPart sign n (c:cs)
        | Ch.isDigit c =  intPart sign (10 * n + fromDigit c) cs
        | c == ' '     =  intPart sign n cs
        | c == '.'     =  decPart sign n 0 cs
        | otherwise    =  tailPart False sign (n, 0) (c:cs)

    decPart :: B.Map DecimalInteger -> DecimalInteger -> Int -> LitDecimal
    decPart sign n p [] = Right $ Decimal (sign n, 10 ^ p) p False
    decPart sign n p (c:cs)
        | Ch.isDigit c = decPart sign (10 * n + fromDigit c) (p + 1) cs
        | c == ' '     =  decPart sign n p cs
        | otherwise    =  tailPart False sign (n, p) (c:cs)

    tailPart :: Bool -> B.Map DecimalInteger -> (DecimalInteger, Int) -> LitDecimal
    tailPart approx sign (n, p) [] = Right $ Decimal (sign n, 10 ^ p) p approx
    tailPart approx sign dec (c:cs) = case c of
        ' '  ->  tailPart approx sign  dec cs
        '-'  ->  tailPart approx minus dec cs
        '+'  ->  tailPart approx id    dec cs
        'a'  ->  tailPart True   sign  dec cs
        'A'  ->  tailPart True   sign  dec cs
        _    ->  Msg.notNumber ccs

fromDigit :: Char -> DecimalInteger
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



-- ----------------------  Writer

decimalString :: Decimal -> String
decimalString (Decimal (n, den) p a)
    | n >= 0     =       digits
    | otherwise  = '-' : digits
    where digits = decimalDigits a p (abs n * 10 ^ p `div` den)

decimalDigits :: Bool -> Int -> DecimalInteger -> String
decimalDigits approx pt
    | pt == 0   = zero . reverse . intPart pt
    | otherwise = zero . reverse . decPart pt
    where
    decPart :: Int -> DecimalInteger -> String
    decPart 0 n = '.' : intPart 0 n
    decPart p n = let (n', d) = quoteDigit n
                  in d : decPart (p - 1) n'

    intPart :: Int -> DecimalInteger -> String
    intPart _ 0 | approx    = " a"
                | otherwise = ""
    intPart 3 n = ' ' : intPart 0 n
    intPart p n = let (n', d) = quoteDigit n
                  in d : intPart (p + 1) n'

    zero ""           = "0"
    zero ds@('.' : _) = '0' : ds
    zero ds           = ds

quoteDigit :: DecimalInteger -> (DecimalInteger, Char)
quoteDigit n = let (n', d) = quotRem n 10
             in (n', Ch.chr $ fromInteger d + Ch.ord '0')

-- map quoteDigit [0..9]
-- map quoteDigit [10..19]
-- map quoteDigit [100..109]

