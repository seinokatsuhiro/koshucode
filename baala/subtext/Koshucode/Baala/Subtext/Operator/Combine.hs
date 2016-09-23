{-# OPTIONS_GHC -Wall #-}

-- | Combinations and variations.

module Koshucode.Baala.Subtext.Operator.Combine
 ( -- * Combination
   or, seq, and,
   not, last, anyNot,

   -- * Variation
   peek, skip, gather,
   sub, (#),
 ) where

import Prelude hiding ( or, seq, and, not, last )

import qualified Koshucode.Baala.Subtext.Fn              as S
import qualified Koshucode.Baala.Subtext.Expr            as S
import qualified Koshucode.Baala.Subtext.Operator.Basic  as S


-- --------------------------------------------  Combination

-- | Alternative match.
or :: [S.Expr a] -> S.Expr a
or []  = S.fail
or [e] = e
or es  = S.ERec $ S.EOr es

-- | Sequential match.
seq :: [S.Expr a] -> S.Expr a
seq []  = S.succ
seq [e] = e
seq es  = S.ERec $ S.ESeq es

-- | Additional condition.
and :: [S.Expr a] -> S.Expr a
and []  = S.succ
and [e] = e
and es  = S.ERec $ S.EAnd es

-- | Inverted condition.
not :: S.Expr a -> S.Expr a
not e = S.ERec $ S.ENot e

-- | Find last match.
last :: S.Expr a -> S.Expr a
last e = S.ERec $ S.ELast e

-- | Any character except for given pattern.
anyNot :: S.Expr a -> S.Expr a
anyNot e = and [S.any, not e]


-- --------------------------------------------  Variation

-- | Match but stay.
peek :: S.Expr a -> S.Expr a
peek = S.ERec . S.EPeek

-- | Turn off gathering match.
skip :: S.Expr a -> S.Expr a
skip = gath False

-- | Turn on gathering match.
gather :: S.Expr a -> S.Expr a
gather = gath True

gath :: Bool -> S.Expr a -> S.Expr a
gath b = S.ERec . S.EGath b

-- | Named submatch.
sub :: S.Name -> S.Expr a -> S.Expr a
sub n = S.ERec . S.ESub n

-- | Named submatch.
(#) :: S.Name -> S.Expr a -> S.Expr a
n # e = sub n e

