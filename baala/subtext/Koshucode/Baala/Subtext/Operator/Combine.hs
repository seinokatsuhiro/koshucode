{-# OPTIONS_GHC -Wall #-}

-- | Combinations and variations.

module Koshucode.Baala.Subtext.Operator.Combine
 ( -- * Combination
   or, seq, and,
   not, last, anyNot,

   -- * Variation
   stay, skip, gather,
   sub, (#),

   -- * Modification
   as, asConst,
   asPrepend, asAppend, asWrap,
 ) where

import Prelude hiding ( or, seq, and, not, last )

import qualified Koshucode.Baala.Subtext.Fn              as S
import qualified Koshucode.Baala.Subtext.Expr            as S
import qualified Koshucode.Baala.Subtext.MinMax          as S
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
--   If expression is zero-repeatable, stirp off the repeatition operator,
--   because zero-repetition can be matched any input.
not :: S.Expr a -> S.Expr a
not = S.ERec . S.ENot . unrep

-- | Strip off zero-repetition operator.
unrep :: S.Expr a -> S.Expr a
unrep = berry f where
    f (S.ERec (S.ERep m e))
        | S.atLeast 0 m  = unrep e
        | otherwise      = S.ERec (S.ERep m e)
    f e = e

-- | Map to berry expression.
berry :: (S.Expr a -> S.Expr a) -> S.Expr a -> S.Expr a
berry f = loop where
    loop (S.ERec (S.ESub n e))   = sub n $ loop e
    loop (S.ERec (S.EGath b e))  = (S.ERec (S.EGath b $ loop e))
    loop e = f e

-- | Find last match.
last :: S.Expr a -> S.Expr a
last e = S.ERec $ S.ELast e

-- | Any character except for given pattern.
anyNot :: S.Expr a -> S.Expr a
anyNot e = and [S.any, not e]


-- --------------------------------------------  Variation

-- | Match but stay.
stay :: S.Expr a -> S.Expr a
stay e = and [S.succ, e]

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


-- --------------------------------------------  Modification

-- | Match with modification.
as :: S.FnAs a -> S.Expr a -> S.Expr a
as f = S.ERec . S.EAs f

-- | Replace to given sequence.
asConst :: [a] -> S.Expr a -> S.Expr a
asConst c = as $ S.Fn "const" $ const c

-- | Add segment to the beginning of matched sequence.
asPrepend :: [a] -> S.Expr a -> S.Expr a
asPrepend a = as (S.Fn "add-prepend" ((a ++) . reverse))

-- | Add segment to the end of matched sequence.
asAppend :: [a] -> S.Expr a -> S.Expr a
asAppend a = as (S.Fn "add-append" ((++ a)  . reverse))

-- | Add segments to the beginning and end of matched sequence.
asWrap :: [a] -> [a]  -> S.Expr a -> S.Expr a
asWrap a b = as (S.Fn "add-wrap" (wrap a b . reverse))

wrap :: [a] -> [a] -> [a] -> [a]
wrap a b x = a ++ x ++ b
