{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Content.Decimal
( -- * Type
  Decimal (..),
  decimalNum,
  fromInt,
  decimalSetPoint,
  decimalDenom,

  -- * Reader
  LitString,
  LitDecimal,
  litDecimal,  

  -- * Writer
  decimalText,

  -- * Arithmetic
  decimalAdd,
  decimalSub,
  decimalMul,
  decimalDiv,
) where

import Data.Char
import Koshucode.Baala.Base



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
    Decimal -> Decimal -> AbOr Decimal

fromInt :: Int -> Decimal
fromInt n = Decimal (n, 1) 0 False

decimalSetPoint :: Int -> AbMap Decimal
decimalSetPoint p2 (Decimal nd1 _ e1) =
    Right $ Decimal nd1 p2 e1

reduceDecimal :: (Int, Int) -> Int -> Bool -> Decimal
reduceDecimal (n, den) = Decimal (n `div` g, den `div` g) where
    g = gcd n den



-- ----------------------  Reader

{-| Make @a@ from a string. -}
type LitString a = AbMap2 String a

type LitDecimal = LitString Decimal

litDecimal :: LitDecimal
litDecimal ccs = headPart id ccs where
    minus x = - x

    headPart _ [] = Right $ Decimal (0, 1) 0 False
    headPart sign (c:cs) = case c of
        ' '  ->  headPart sign  cs
        '-'  ->  headPart minus cs
        '+'  ->  headPart id    cs
        _    ->  intPart sign 0 (c:cs)

    intPart :: Map Int -> Int -> LitDecimal
    intPart sign n [] = Right $ Decimal (sign n, 1) 0 False
    intPart sign n (c:cs)
        | isDigit c  =  intPart sign (10 * n + fromDigit c) cs
        | c == ' '   =  intPart sign n cs
        | c == '.'   =  decPart sign n 0 cs
        | otherwise  =  tailPart False sign (n, 0) (c:cs)

    decPart :: Map Int -> Int -> Int -> LitDecimal
    decPart sign n p [] = Right $ Decimal (sign n, 1) p False
    decPart sign n p (c:cs)
        | isDigit c  =  decPart sign (10 * n + fromDigit c) (p + 1) cs
        | c == ' '   =  decPart sign n p cs
        | otherwise  =  tailPart False sign (n, p) (c:cs)

    tailPart :: Bool -> Map Int -> (Int, Int) -> LitDecimal
    tailPart approx sign (n, p) [] = Right $ Decimal (sign n, 1) p approx
    tailPart approx sign dec (c:cs) = case c of
        ' '  ->  tailPart approx sign  dec cs
        '-'  ->  tailPart approx minus dec cs
        '+'  ->  tailPart approx id    dec cs
        'a'  ->  tailPart True   sign  dec cs
        'A'  ->  tailPart True   sign  dec cs
        _    ->  Left $ AbortNotNumber ccs

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
fromDigit _   = bug



-- ----------------------  Writer

decimalText :: Decimal -> String
decimalText (Decimal (n, den) p _)
    | p == 0    = show $ n `div` den
    | p >  0    = point p (show $ n `div` den)
    | otherwise = "()"

point :: Int -> Map String
point p ds = point2 (length ds - p) ds

point2 :: Int -> Map String
point2 p (d:ds)
    | p == 1  =  d : '.' : point2 (p - 1) ds
    | p >  0  =  d : point2 (p - 1) ds
point2 0 ds = ds
point2 _ _ = bug



-- ----------------------  Arithmetic

decimalAdd :: DecimalBinary
decimalAdd (Decimal (n1, den1) p1 e1) (Decimal (n2, den2) _ e2)
    | den1 == den2 = Right $ reduceDecimal (n1 + n2, den1) p1 e3
    | otherwise    = Right $ reduceDecimal (n3, den3) p1 e3
    where n3    =  (n1 * den2) + (n2 * den1)
          den3  =  den1 * den2
          e3    =  e1 || e2

decimalSub :: DecimalBinary
decimalSub (Decimal (n1, den1) p1 e1) (Decimal (n2, den2) p2 e2)
    | p1 == p2 =
        if den1 == den2
        then Right $ reduceDecimal (n1 - n2, den1) p1 e3
        else Right $ reduceDecimal (n3, den3) p1 e3
    | otherwise =  bug
    where n3    =  (n1 * den2) - (n2 * den1)
          den3  =  den1 * den2
          e3    =  e1 || e2

decimalMul :: DecimalBinary
decimalMul (Decimal (n1, den1) p1 e1) (Decimal (n2, den2) p2 e2)
    = Right $ Decimal (n3, den3) p3 e3
    where n3    =  (n1   `div` g1) * (n2   `div` g2)
          den3  =  (den2 `div` g1) * (den1 `div` g2)
          g1    =  gcd n1 den2
          g2    =  gcd n2 den1
          p3    =  p1 + p2
          e3    =  e1 || e2

decimalDiv :: DecimalBinary
decimalDiv dec1 (Decimal (n2, den2) p2 e2)
    = decimalMul dec1 (Decimal (den2, n2) (- p2) e2)

