{-# OPTIONS_GHC -Wall #-}

-- | Extra functions for rational numbers.

module Koshucode.Baala.Data.Type.Decimal.Rational
  ( -- * Shorthand
    (%%), (//.), (//),
    ratioHalf, ratioFracle,

    -- * Round and truncate
    ratioRoundAt, ratioRoundEvenAt,
    ratioRoundPer, ratioRoundEvenPer,
    ratioTruncAt, ratioTruncPer,
    ratioRoundOutAt, ratioRoundOutPer,

    -- * Floor and ceiling
    ratioFloorAt, ratioFloorPer,
    ratioCeilAt, ratioCeilPer,

    -- * Quotient and remainder
    ratioQuo, ratioRem, ratioQuoRem,
  ) where

import qualified Data.Ratio                     as R
import qualified Koshucode.Baala.Overture       as O
import qualified Koshucode.Baala.Base           as B


-- --------------------------------------------  Shorthand

-- | 'Rational' version of 'R.%'.
(%%) :: Integer -> Integer -> Rational
(%%) = (R.%)

-- | Integer part and proper fraction part of ratio of two numbers,
--   i.e., /x/ '//.' /y/ == (/integer/, /proper-fraction/) of /x/ 'R.%' /y/.
(//.) :: (Integral n) => n -> n -> (n, R.Ratio n)
x //. y = (q, r R.% y) where (q, r) = x // y

-- | Synonym of 'quotRem'.
(//) :: (Integral n) => n -> n -> (n, n)
(//) = quotRem

-- | 1 'R.%' 2
ratioHalf :: (Integral n) => R.Ratio n
ratioHalf = 1 R.% 2

-- | Unit rational number for decimal fractional length.
ratioFracle :: (Integral i, Integral n) => i -> R.Ratio n
ratioFracle l = 10 ^^ (- l)


-- --------------------------------------------  Round and truncate

-- let sh = show . fromRational
-- let test l r = putStrLn $ sh (ratioRoundAt l r) ++ " | " ++ sh (ratioRoundEvenAt l r) ++ "  <-  " ++ sh r
-- let test l r = putStrLn $ sh (ratioTruncAt l r) ++ "  <-  " ++ sh r
-- test (0 :: Int)  `mapM_` ((%% 4) <$> [-8 .. 15])
-- test (-2 :: Int) `mapM_` ((%% 1) <$> [-100, -50 .. 500])

ratioRoundPerBy :: (Integral n) => (n -> B.Bin (R.Ratio n)) -> B.Bin (R.Ratio n)
ratioRoundPerBy conv per r = signum r * conv int frac trunc where
    (int, frac)  = properFraction (abs r / per)
    trunc        = (int R.% 1) * per

-- | Round rational number at decimal fractional length.
ratioRoundAt :: (Integral l, Integral n) => l -> O.Map (R.Ratio n)
ratioRoundAt = ratioRoundPer . ratioFracle

-- | Round-to-even rational number at decimal fractional length.
ratioRoundEvenAt :: (Integral l, Integral n) => l -> O.Map (R.Ratio n)
ratioRoundEvenAt = ratioRoundEvenPer . ratioFracle

-- | Round rational number per unit rational number.
ratioRoundPer :: (Integral n) => B.Bin (R.Ratio n)
ratioRoundPer per = ratioRoundPerBy conv per where
    conv _ frac trunc | frac >= ratioHalf  = trunc + per
                      | otherwise          = trunc

-- | Round-to-even rational number per unit rational number.
ratioRoundEvenPer :: (Integral n) => B.Bin (R.Ratio n)
ratioRoundEvenPer per = ratioRoundPerBy conv per where
    conv int frac trunc =
        case frac `compare` ratioHalf of
          GT -> trunc + per
          EQ -> trunc + (if even int then 0 else per)
          LT -> trunc

-- | Truncate rational number at decimal fractional length.
ratioTruncAt :: (Integral l, Integral n) => l -> O.Map (R.Ratio n)
ratioTruncAt = ratioTruncPer . ratioFracle

-- | Truncate rational number per unit rational number.
ratioTruncPer :: (Integral n) => B.Bin (R.Ratio n)
ratioTruncPer = ratioRoundPerBy conv where
    conv _ _ trunc = trunc

-- | Round out (toward infinity) rational number at decimal fractional length.
ratioRoundOutAt :: (Integral l, Integral n) => l -> O.Map (R.Ratio n)
ratioRoundOutAt = ratioRoundOutPer . ratioFracle

-- | Round out (toward infinity) rational number per unit rational number.
ratioRoundOutPer :: (Integral n) => B.Bin (R.Ratio n)
ratioRoundOutPer per = ratioRoundPerBy conv per where
    conv _ frac trunc | frac > 0  = trunc + per
                      | otherwise = trunc


-- --------------------------------------------  Floor and ceiling

-- let sh = show . fromRational
-- let test l r = putStrLn $ sh (ratioFloorAt l r) ++ "  |  " ++ sh (ratioCeilAt l r) ++ "  <-  " ++ sh r
-- test (0 :: Int)  `mapM_` ((%% 4) <$> [-8 .. 15])

ratioFloorPerBy :: (Integral n) => B.Bin (R.Ratio n) -> B.Bin (R.Ratio n)
ratioFloorPerBy conv per r = conv frac trunc where
    (int, frac)  = properFraction (r / per)
    trunc        = (int R.% 1) * per

-- | Floor rational number at decimal fractional length.
ratioFloorAt :: (Integral l, Integral n) => l -> O.Map (R.Ratio n)
ratioFloorAt = ratioFloorPer . ratioFracle

-- | Floor rational number per unit rational number.
ratioFloorPer :: (Integral n) => B.Bin (R.Ratio n)
ratioFloorPer per = ratioFloorPerBy conv per where
    conv frac trunc | frac < 0   = trunc - per
                    | otherwise  = trunc

-- | Ceiling rational number at decimal fractional length.
ratioCeilAt :: (Integral l, Integral n) => l -> O.Map (R.Ratio n)
ratioCeilAt = ratioCeilPer . ratioFracle

-- | Ceiling rational number per unit rational number.
ratioCeilPer :: (Integral n) => B.Bin (R.Ratio n)
ratioCeilPer per = ratioFloorPerBy conv per where
    conv frac trunc | frac > 0   = trunc + per
                    | otherwise  = trunc

-- --------------------------------------------  Quotient and remainder

ratioQuo :: (Integral n) => B.Bin (R.Ratio n)
ratioQuo x y = fst $ ratioQuoRem x y

ratioRem :: (Integral n) => B.Bin (R.Ratio n)
ratioRem x y = snd $ ratioQuoRem x y

ratioQuoRem :: (Integral n) => R.Ratio n -> R.Ratio n -> (R.Ratio n, R.Ratio n)
ratioQuoRem = ratioQuoRem'

ratioQuoRem' :: (Num n, RealFrac r) => r -> r -> (n, r)
ratioQuoRem' x y = (fromInteger i, y * f) where
    (i, f) = properFraction (x / y)

