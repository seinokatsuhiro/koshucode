{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Decimal
( -- * Type
  Decimal (..),
  decimalNum,
  intDecimal,
  decimalSetPoint,
  decimalDenom,
  isDecimalZero,

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

import qualified Data.Char as Ch
import Control.Monad
import qualified Koshucode.Baala.Base.Abort   as B
import qualified Koshucode.Baala.Base.Prelude as B



-- ----------------------  Type

data Decimal = Decimal 
    { decimalRatio   :: (Int, Int)
    , decimalLength  :: Int
    , decimalApprox  :: Bool
    } deriving (Show, Eq, Ord)

decimalNum :: Decimal -> Int
decimalNum = fst . decimalRatio

decimalDenom :: Decimal -> Int
decimalDenom = snd . decimalRatio

type DecimalBinary =
    Decimal -> Decimal -> B.Ab Decimal

intDecimal :: Int -> Decimal
intDecimal n = Decimal (n, 1) 0 False

decimalSetPoint :: Int -> B.AbMap Decimal
decimalSetPoint p2 (Decimal nd1 _ e1) =
    Right $ Decimal nd1 p2 e1

reduceDecimal :: (Int, Int) -> Int -> Bool -> Decimal
reduceDecimal (n, den) = Decimal (n `div` g, den `div` g) where
    g = gcd n den

isDecimalZero :: Decimal -> Bool
isDecimalZero (Decimal (n, _) _ _)  =  n == 0



-- ----------------------  Reader

{-| Make @a@ from a string. -}
type LitString a = String -> B.Ab a

type LitDecimal = LitString Decimal

litDecimal :: LitDecimal
litDecimal ccs = headPart id ccs where
    minus x = - x

    headPart _ [] = Left $ B.abortBy $ B.ASNotNumber []
    headPart sign (c:cs) = case c of
        ' '  ->  headPart sign  cs
        '-'  ->  headPart minus cs
        '+'  ->  headPart id    cs
        _    ->  intPart sign 0 (c:cs)

    intPart :: B.Map Int -> Int -> LitDecimal
    intPart sign n [] = Right $ Decimal (sign n, 1) 0 False
    intPart sign n (c:cs)
        | Ch.isDigit c =  intPart sign (10 * n + fromDigit c) cs
        | c == ' '     =  intPart sign n cs
        | c == '.'     =  decPart sign n 0 cs
        | otherwise    =  tailPart False sign (n, 0) (c:cs)

    decPart :: B.Map Int -> Int -> Int -> LitDecimal
    decPart sign n p [] = Right $ Decimal (sign n, 10 ^ p) p False
    decPart sign n p (c:cs)
        | Ch.isDigit c = decPart sign (10 * n + fromDigit c) (p + 1) cs
        | c == ' '     =  decPart sign n p cs
        | otherwise    =  tailPart False sign (n, p) (c:cs)

    tailPart :: Bool -> B.Map Int -> (Int, Int) -> LitDecimal
    tailPart approx sign (n, p) [] = Right $ Decimal (sign n, 10 ^ p) p approx
    tailPart approx sign dec (c:cs) = case c of
        ' '  ->  tailPart approx sign  dec cs
        '-'  ->  tailPart approx minus dec cs
        '+'  ->  tailPart approx id    dec cs
        'a'  ->  tailPart True   sign  dec cs
        'A'  ->  tailPart True   sign  dec cs
        _    ->  Left $ B.abortBy $ B.ASNotNumber ccs

fromDigit :: Char -> Int
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

decimalDigits :: Bool -> Int -> Int -> String
decimalDigits approx pt
    | pt == 0   = zero . reverse . intPart pt
    | otherwise = zero . reverse . decPart pt
    where
    decPart :: Int -> Int -> String
    decPart 0 n = '.' : intPart 0 n
    decPart p n = let (n', d) = quoteDigit n
                  in d : decPart (p - 1) n'

    intPart :: Int -> Int -> String
    intPart _ 0 | approx    = " a"
                | otherwise = ""
    intPart 3 n = ' ' : intPart 0 n
    intPart p n = let (n', d) = quoteDigit n
                  in d : intPart (p + 1) n'

    zero ""           = "0"
    zero ds@('.' : _) = '0' : ds
    zero ds           = ds

quoteDigit :: Int -> (Int, Char)
quoteDigit n = let (n', d) = quotRem n 10
             in (n', Ch.chr $ d + Ch.ord '0')

-- map quoteDigit [0..9]
-- map quoteDigit [10..19]
-- map quoteDigit [100..109]



-- ----------------------  Arithmetic

decimalAdd :: DecimalBinary
decimalAdd d1@(Decimal (n1, den1) p1 a1)
           d2@(Decimal (n2, den2) p2 a2)
    | p1 /= p2     = Left  $ B.abortBy $ B.ACHeteroDecimal txt1 txt2
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

decimalQR :: (Int -> Int -> Int) -> DecimalBinary
decimalQR qr
          d1@(Decimal (n1, den1) p1 a1)
          d2@(Decimal (n2, den2) p2 a2)
    | p1 /= p2     = Left  $ B.abortBy $ B.ACHeteroDecimal txt1 txt2
    | n2 == 0      = Left  $ B.abortBy $ B.ACDivideByZero
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
decimalSum = foldM decimalAdd $ intDecimal 0

