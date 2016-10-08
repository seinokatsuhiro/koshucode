{-# OPTIONS_GHC -Wall #-}

-- | Repetitive operators.

module Koshucode.Baala.Subtext.Operator.Repeat
 ( -- * Basic repetition
   repeat, repeatTimes, repeatExpr,
   min, max, minMax,

   -- * Derived repetition
   many, many1, maybe,
   anySeq, before, sep,
 ) where

import Prelude hiding ( repeat, min, max, maybe )

import qualified Koshucode.Baala.Subtext.Expr                as S
import qualified Koshucode.Baala.Subtext.MinMax              as S
import qualified Koshucode.Baala.Subtext.Operator.Basic      as S
import qualified Koshucode.Baala.Subtext.Operator.Combine    as S


-- ----------------------  Basic repetition

-- | Repetition with min-max parameter.
repeat :: S.MinMax -> S.Expr a -> S.Expr a
repeat m = S.ERec . S.ERep m

-- | Extract min-max parameter.
repeatTimes :: S.Expr a -> Maybe S.MinMax
repeatTimes (S.ERec (S.ERep t _))  = Just t
repeatTimes _                      = Nothing

-- | Extract internal expression.
repeatExpr :: S.Expr a -> Maybe (S.Expr a)
repeatExpr (S.ERec (S.ERep _ e))   = Just e
repeatExpr _                       = Nothing

-- | Repetition with lower bound.
min :: Int -> S.Expr a -> S.Expr a
min a = repeat $ S.Min a

-- | Repetition upper bound.
max :: Int -> S.Expr a -> S.Expr a
max = minMax 0

-- | Repetition with lower and upper bound.
minMax :: Int -> Int -> S.Expr a -> S.Expr a
minMax a b = repeat $ S.MinMax a b


-- ----------------------  Derived repetition

-- | Match zero or more times.
many :: S.Expr a -> S.Expr a
many = min 0

-- | Match one or more times.
many1 :: S.Expr a -> S.Expr a
many1 = min 1

-- | Match zero or one times.
maybe :: S.Expr a -> S.Expr a
maybe = minMax 0 1

-- | Many of any.
anySeq :: S.Expr a
anySeq = many S.any

-- | Match before given expression.
before :: S.Expr a -> S.Expr a
before = many . S.anyNot

-- | X-separated values.
sep :: S.Expr a -> S.Expr a -> S.Expr a
sep x e = S.seq [many $ S.seq [e, x], maybe e]
