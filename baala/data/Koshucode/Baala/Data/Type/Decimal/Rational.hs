{-# OPTIONS_GHC -Wall #-}

-- | Extra functions for rational numbers.

module Koshucode.Baala.Data.Type.Decimal.Rational
  ( -- * Shorthand
    (%%), (//.), (//),
    ratioHalf, ratioFracl,

    -- * Round
    ratioRoundAt, ratioRoundEvenAt,
    ratioRoundPer, ratioRoundEvenPer,
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


-- --------------------------------------------  Round

-- let sh = show . fromRational
-- let test l r = putStrLn $ sh (ratioRoundAt l r) ++ " | " ++ sh (ratioRoundEvenAt l r) ++ "  <-  " ++ sh r
-- test (0 :: Int)  `mapM_` ((%% 4) <$> [-8 .. 15])
-- test (-2 :: Int) `mapM_` ((%% 1) <$> [-100, -50 .. 500])

-- | Round rational number at decimal fractional length.
ratioRoundAt :: (Integral l, Integral n) => l -> B.Map (R.Ratio n)
ratioRoundAt l = ratioRoundPer $ ratioFracl l

-- | Round-to-even rational number at decimal fractional length.
ratioRoundEvenAt :: (Integral l, Integral n) => l -> B.Map (R.Ratio n)
ratioRoundEvenAt l = ratioRoundEvenPer $ ratioFracl l

-- | Round rational number per unit rational number.
ratioRoundPer :: (Integral n) => B.Bin (R.Ratio n)
ratioRoundPer per = ratioRoundPerBy conv per where
    conv _ trunc f | f >= ratioHalf  = trunc + per
                   | otherwise       = trunc

-- | Round-to-even rational number per unit rational number.
ratioRoundEvenPer :: (Integral n) => B.Bin (R.Ratio n)
ratioRoundEvenPer per = ratioRoundPerBy conv per where
    conv i trunc f = case f `compare` ratioHalf of
                       GT -> trunc + per
                       EQ -> trunc + (if even i then 0 else per)
                       LT -> trunc

ratioRoundPerBy :: (Integral n) => (n -> B.Bin (R.Ratio n)) -> B.Bin (R.Ratio n)
ratioRoundPerBy conv per r = signum r * conv i trunc f where
    (i, f)  = properFraction (abs r / per)
    trunc   = (i R.% 1) * per

