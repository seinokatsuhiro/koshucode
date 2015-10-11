{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Decimal
  ( -- * Type
    Decimal (..),
    DecimalInteger,
    isDecimalZero,
    decimalNum, decimalDenom,
    decimalSetPoint,
    intDecimal,
    decimalFromRealFloat, decimalToRealFloat,
  
    -- * Reader
    LitString,
    LitDecimal,
    litDecimal,  
  
    -- * Writer
    decimalString,
  
    -- * Arithmetic
    DecimalBinary,
    decimalAdd,
    decimalSub,
    decimalMul,
    decimalDiv,
    decimalQuo,
    decimalRem,
  
    decimalRevsign,
    decimalRevratio,
    decimalAbs,
    decimalSum,
  ) where

import qualified Data.Char                         as Ch
import qualified Data.Ratio                        as R
import qualified Control.Monad                     as M
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Type.Message as Msg



-- ----------------------  Type

type DecimalInteger = Integer

data Decimal = Decimal 
    { decimalRatio   :: (DecimalInteger, DecimalInteger)
    , decimalLength  :: Int
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

-- | Change precision.
decimalSetPoint :: Int -> B.AbMap Decimal
decimalSetPoint p2 (Decimal r1 _ e1) =
    Right $ Decimal r1 p2 e1

reduceDecimal :: (DecimalInteger, DecimalInteger) -> Int -> Bool -> Decimal
reduceDecimal (n, den) = Decimal (n `div` g, den `div` g) where
    g = gcd n den

-- | Convert integral number to decimal number.
intDecimal :: DecimalInteger -> Decimal
intDecimal n = Decimal (n, 1) 0 False

-- | Convert real-float number to decimal number.
decimalFromRealFloat :: (RealFloat n) => Int -> n -> Decimal
decimalFromRealFloat k n = Decimal (fromInteger num, fromInteger den) k False where
    r   = toRational n
    num = R.numerator   r
    den = R.denominator r

-- | Convert decimal number to read-float number.
decimalToRealFloat :: (RealFloat n) => Decimal -> n
decimalToRealFloat (Decimal { decimalRatio = (num, den)}) =
    fromRational $ toRational $ (num R.% den)


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



-- ----------------------  Arithmetic

type DecimalBinary =
    Decimal -> Decimal -> B.Ab Decimal

decimalAdd :: DecimalBinary
decimalAdd d1@(Decimal (n1, den1) p1 a1)
           d2@(Decimal (n2, den2) p2 a2)
    | p1 /= p2     = Msg.heteroDecimal txt1 txt2
    | den1 == den2 = Right $ reduceDecimal (n1 + n2, den1) p1 a3
    | otherwise    = Right $ reduceDecimal (n3, den3) p1 a3
    where n3    =  (n1 * den2) + (n2 * den1)
          den3  =  den1 * den2
          a3    =  a1 || a2
          txt1  =  decimalString d1
          txt2  =  decimalString d2

decimalSub :: DecimalBinary
decimalSub d1 d2 = decimalAdd d1 $ decimalRevsign d2

decimalMul :: DecimalBinary
decimalMul (Decimal (n1, den1) p1 a1) (Decimal (n2, den2) p2 a2)
    = Right $ Decimal (n3, den3) p3 a3
    where n3    =  (n1   `div` g1) * (n2   `div` g2)
          den3  =  (den2 `div` g1) * (den1 `div` g2)
          g1    =  gcd n1 den2
          g2    =  gcd n2 den1
          p3    =  max p1 p2
          a3    =  a1 || a2

decimalDiv :: DecimalBinary
decimalDiv dec1 dec2
    = decimalMul dec1 $ decimalRevratio dec2

decimalQuo :: DecimalBinary
decimalQuo = decimalQR quot

decimalRem :: DecimalBinary
decimalRem = decimalQR rem

decimalQR :: (DecimalInteger -> DecimalInteger -> DecimalInteger) -> DecimalBinary
decimalQR qr
          d1@(Decimal (n1, den1) p1 a1)
          d2@(Decimal (n2, den2) p2 a2)
    | p1 /= p2     = Msg.heteroDecimal txt1 txt2
    | n2 == 0      = Msg.divideByZero
    | otherwise    = Right $ Decimal (n3, 1) p1 a3
    where n3    =  (n1 * den2) `qr` (n2 * den1)
          a3    =  a1 || a2
          txt1  =  decimalString d1
          txt2  =  decimalString d2

-- ----------------------

decimalRevsign :: B.Map Decimal
decimalRevsign (Decimal (n, den) p a) = Decimal (- n, den) p a

decimalRevratio :: B.Map Decimal
decimalRevratio (Decimal (n, den) p a) = Decimal (den, n) p a

decimalAbs :: B.Map Decimal
decimalAbs (Decimal (n, den) p a) = Decimal (abs n, den) p a

decimalSum :: [Decimal] -> B.Ab Decimal
decimalSum = M.foldM decimalAdd $ intDecimal 0
