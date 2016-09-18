{-# OPTIONS_GHC -Wall #-}

-- | Combinations and variations.

module Koshucode.Baala.Subtext.Operator.Combine
 ( -- * Combination
   or, seq, not, not',

   -- * Variation
   peek, skip, gather,
   sub, (#),
 ) where

import Prelude hiding ( or, seq, not )

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

-- | Match with exceptions.
not :: S.Expr a -> [S.Expr a] -> S.Expr a
not e [] = e
not e es = S.ERec $ S.ENot e es

-- | List version of 'not'.
--
--   * @not' [/E/, /E1/, /E2/]@ == @not /E/ [/E1/, /E2/]@
--   * @not' [/E/]@ == @/E/@
--   * @not' []@ == @seq []@ == @succ@

not' :: [S.Expr a] -> S.Expr a
not' []        = S.succ
not' [e]       = e
not' (e : es)  = not e es


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

