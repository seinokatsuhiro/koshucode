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

import qualified Koshucode.Baala.Subtext.Expr                as T
import qualified Koshucode.Baala.Subtext.MinMax              as T
import qualified Koshucode.Baala.Subtext.Operator.Basic      as T
import qualified Koshucode.Baala.Subtext.Operator.Combine    as T


-- ----------------------  Basic repetition

-- | Repetition with min-max parameter.
repeat :: T.MinMax -> T.Expr a -> T.Expr a
repeat m = T.ERec . T.ERep m

-- | Extract min-max parameter.
repeatTimes :: T.Expr a -> Maybe T.MinMax
repeatTimes (T.ERec (T.ERep t _))  = Just t
repeatTimes _                      = Nothing

-- | Extract internal expression.
repeatExpr :: T.Expr a -> Maybe (T.Expr a)
repeatExpr (T.ERec (T.ERep _ e))   = Just e
repeatExpr _                       = Nothing

-- | Repetition with lower bound.
min :: Int -> T.Expr a -> T.Expr a
min a = repeat $ T.Min a

-- | Repetition upper bound.
max :: Int -> T.Expr a -> T.Expr a
max = minMax 0

-- | Repetition with lower and upper bound.
minMax :: Int -> Int -> T.Expr a -> T.Expr a
minMax a b = repeat $ T.MinMax a b


-- ----------------------  Derived repetition

-- | Match zero or more times.
many :: T.Expr a -> T.Expr a
many = min 0

-- | Match one or more times.
many1 :: T.Expr a -> T.Expr a
many1 = min 1

-- | Match zero or one times.
maybe :: T.Expr a -> T.Expr a
maybe = minMax 0 1

-- | Many of any.
anySeq :: T.Expr a
anySeq = many T.any

-- | Match before given expression.
before :: T.Expr a -> T.Expr a
before = many . T.anyNot

-- | X-separated values.
sep :: T.Expr a -> T.Expr a -> T.Expr a
sep x e = T.seq [many $ T.seq [e, x], maybe e]
