{-# OPTIONS_GHC -Wall #-}

-- | Combinations and variations.

module Koshucode.Baala.Subtext.Operator.Combine
 ( -- * Combination
   or, seq, not,

   -- * Variation
   peek, skip, gather,
   sub, (#),
 ) where

import Prelude hiding ( or, seq, not )

import qualified Koshucode.Baala.Subtext.Fn    as S
import qualified Koshucode.Baala.Subtext.Expr  as S


-- --------------------------------------------  Combination

-- | Alternative match.
or :: [S.Expr a] -> S.Expr a
or = S.ERec . S.EOr

-- | Sequential match.
seq :: [S.Expr a] -> S.Expr a
seq = S.ERec . S.ESeq

-- | Match with exceptions.
not :: S.Expr a -> [S.Expr a] -> S.Expr a
not e es = S.ERec $ S.ENot e es


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

