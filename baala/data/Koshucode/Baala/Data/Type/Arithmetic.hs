{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Arithmetic
  ( DecimalBinary,
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

import qualified Control.Monad                     as M
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Type.Decimal as D
import qualified Koshucode.Baala.Data.Type.Message as Msg

type DecimalBinary =
    D.Decimal -> D.Decimal -> B.Ab D.Decimal

decimalAdd :: DecimalBinary
decimalAdd d1@(D.Decimal (n1, den1) p1 a1)
           d2@(D.Decimal (n2, den2) p2 a2)
    | p1 /= p2     = Msg.heteroDecimal txt1 txt2
    | den1 == den2 = Right $ D.reduceDecimal (n1 + n2, den1) p1 a3
    | otherwise    = Right $ D.reduceDecimal (n3, den3) p1 a3
    where n3    =  (n1 * den2) + (n2 * den1)
          den3  =  den1 * den2
          a3    =  a1 || a2
          txt1  =  D.decimalString d1
          txt2  =  D.decimalString d2

decimalSub :: DecimalBinary
decimalSub d1 d2 = decimalAdd d1 $ decimalRevsign d2

decimalMul :: DecimalBinary
decimalMul (D.Decimal (n1, den1) p1 a1) (D.Decimal (n2, den2) p2 a2)
    = Right $ D.Decimal (n3, den3) p3 a3
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

decimalQR :: (D.DecimalInteger -> D.DecimalInteger -> D.DecimalInteger) -> DecimalBinary
decimalQR qr
          d1@(D.Decimal (n1, den1) p1 a1)
          d2@(D.Decimal (n2, den2) p2 a2)
    | p1 /= p2     = Msg.heteroDecimal txt1 txt2
    | n2 == 0      = Msg.divideByZero
    | otherwise    = Right $ D.Decimal (n3, 1) p1 a3
    where n3    =  (n1 * den2) `qr` (n2 * den1)
          a3    =  a1 || a2
          txt1  =  D.decimalString d1
          txt2  =  D.decimalString d2

-- ----------------------

decimalRevsign :: B.Map D.Decimal
decimalRevsign (D.Decimal (n, den) p a) = D.Decimal (- n, den) p a

decimalRevratio :: B.Map D.Decimal
decimalRevratio (D.Decimal (n, den) p a) = D.Decimal (den, n) p a

decimalAbs :: B.Map D.Decimal
decimalAbs (D.Decimal (n, den) p a) = D.Decimal (abs n, den) p a

decimalSum :: [D.Decimal] -> B.Ab D.Decimal
decimalSum = M.foldM decimalAdd $ D.integralDecimal (0 :: Int)

