{-# OPTIONS_GHC -Wall #-}

-- | Extra functions for rational numbers.

module Koshucode.Baala.Data.Type.Decimal.Rational
  ( -- * Shorthand
    (%%), (//.), (//),
    ratioHalf, ratioFracl,

    -- * Round and truncate
    ratioRoundAt, ratioRoundEvenAt,
    ratioRoundPer, ratioRoundEvenPer,
    ratioTruncAt, ratioTruncPer,
  ) where

import qualified Data.Ratio                     as R
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
ratioFracl :: (Integral i, Integral n) => i -> R.Ratio n
ratioFracl l = 10 ^^ (- l)


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
ratioRoundAt :: (Integral l, Integral n) => l -> B.Map (R.Ratio n)
ratioRoundAt = ratioRoundPer . ratioFracl

-- | Round-to-even rational number at decimal fractional length.
ratioRoundEvenAt :: (Integral l, Integral n) => l -> B.Map (R.Ratio n)
ratioRoundEvenAt = ratioRoundEvenPer . ratioFracl

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
ratioTruncAt :: (Integral l, Integral n) => l -> B.Map (R.Ratio n)
ratioTruncAt = ratioTruncPer . ratioFracl

-- | Truncate rational number per unit rational number.
ratioTruncPer :: (Integral n) => B.Bin (R.Ratio n)
ratioTruncPer = ratioRoundPerBy conv where
    conv _ _ trunc = trunc

