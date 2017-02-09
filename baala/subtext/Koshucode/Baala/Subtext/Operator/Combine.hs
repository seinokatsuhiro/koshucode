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

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Overture.Fn             as O
import qualified Koshucode.Baala.Subtext.Expr            as T
import qualified Koshucode.Baala.Subtext.MinMax          as T
import qualified Koshucode.Baala.Subtext.Operator.Basic  as T


-- --------------------------------------------  Combination

-- | Alternative match.
or :: [T.Expr as a] -> T.Expr as a
or []  = T.fail
or [e] = e
or es  = T.ERec $ T.EOr es

-- | Sequential match.
seq :: [T.Expr as a] -> T.Expr as a
seq []  = T.succ
seq [e] = e
seq es  = T.ERec $ T.ESeq es

-- | Additional condition.
and :: [T.Expr as a] -> T.Expr as a
and []  = T.succ
and [e] = e
and es  = T.ERec $ T.EAnd es

-- | Inverted condition.
--   If expression is zero-repeatable, stirp off the repeatition operator,
--   because zero-repetition can be matched any input.
not :: T.Expr as a -> T.Expr as a
not = T.ERec . T.ENot . unrep

-- | Strip off zero-repetition operator.
unrep :: T.Expr as a -> T.Expr as a
unrep = berry f where
    f (T.ERec (T.ERep m e))
        | T.atLeast 0 m  = unrep e
        | otherwise      = T.ERec (T.ERep m e)
    f e = e

-- | Map to berry expression.
berry :: (T.Expr as a -> T.Expr as a) -> T.Expr as a -> T.Expr as a
berry f = loop where
    loop (T.ERec (T.ESub n e))   = sub n $ loop e
    loop (T.ERec (T.EGath b e))  = (T.ERec (T.EGath b $ loop e))
    loop e = f e

-- | Find last match.
last :: T.Expr as a -> T.Expr as a
last e = T.ERec $ T.ELast e

-- | Any character except for given pattern.
anyNot :: T.Expr as a -> T.Expr as a
anyNot e = and [T.any, not e]


-- --------------------------------------------  Variation

-- | Match but stay.
stay :: T.Expr as a -> T.Expr as a
stay e = and [T.succ, e]

-- | Turn off gathering match.
skip :: T.Expr as a -> T.Expr as a
skip = gath False

-- | Turn on gathering match.
gather :: T.Expr as a -> T.Expr as a
gather = gath True

gath :: Bool -> T.Expr as a -> T.Expr as a
gath b = T.ERec . T.EGath b

-- | Named submatch.
sub :: O.Name -> T.Expr as a -> T.Expr as a
sub n = T.ERec . T.ESub n

-- | Named submatch.
(#) :: O.Name -> T.Expr as a -> T.Expr as a
n # e = sub n e


-- --------------------------------------------  Modification

-- | Match with modification.
as :: T.FnAs as a -> T.Expr as a -> T.Expr as a
as f = T.ERec . T.EAs f

-- | Replace to given sequence.
asConst :: (O.List as a) => as -> T.Expr as a -> T.Expr as a
asConst c = as $ O.Fn "const" $ const c

-- | Add segment to the beginning of matched sequence.
asPrepend :: (O.List as a) => as -> T.Expr as a -> T.Expr as a
asPrepend a = as (O.Fn "add-prepend" ((a O.++) . O.reverse))

-- | Add segment to the end of matched sequence.
asAppend :: (O.List as a) => as -> T.Expr as a -> T.Expr as a
asAppend a = as (O.Fn "add-append" ((O.++ a)  . O.reverse))

-- | Add segments to the beginning and end of matched sequence.
asWrap :: (O.List as a) => as -> as -> T.Expr as a -> T.Expr as a
asWrap a b = as (O.Fn "add-wrap" (wrap a b . O.reverse))

wrap :: (Monoid as) => as -> as -> as -> as
wrap a b x = a O.++ x O.++ b
