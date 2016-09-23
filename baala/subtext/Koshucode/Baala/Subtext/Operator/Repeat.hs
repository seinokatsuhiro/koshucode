{-# OPTIONS_GHC -Wall #-}

-- | Repetitive operators.

module Koshucode.Baala.Subtext.Operator.Repeat
 ( -- * Repetitive
   min, max, minMax,
   many, many1, maybe,
   before, sep,
 ) where

import Prelude hiding ( min, max, maybe )

import qualified Koshucode.Baala.Subtext.Expr                as S
import qualified Koshucode.Baala.Subtext.MinMax              as S
import qualified Koshucode.Baala.Subtext.Operator.Combine    as S

-- | Repetition with lower bound.
min :: Int -> S.Expr a -> S.Expr a
min a = S.ERec . S.ERep (S.Min a)

-- | Repetition upper bound.
max :: Int -> S.Expr a -> S.Expr a
max = minMax 0

-- | Repetition with lower and upper bound.
minMax :: Int -> Int -> S.Expr a -> S.Expr a
minMax a b = S.ERec . S.ERep (S.MinMax a b)

-- | Match zero or more times.
many :: S.Expr a -> S.Expr a
many = min 0

-- | Match one or more times.
many1 :: S.Expr a -> S.Expr a
many1 = min 1

-- | Match zero or one times.
maybe :: S.Expr a -> S.Expr a
maybe = minMax 0 1

-- | Match before given expression.
before :: S.Expr a -> S.Expr a
before = many . S.anyNot

-- | X-separated values.
sep :: S.Expr a -> S.Expr a -> S.Expr a
sep x e = S.seq [many $ S.seq [e, x], maybe e]
