{-# OPTIONS_GHC -Wall #-}

-- | Basic operators.

module Koshucode.Baala.Subtext.Operator.Basic
 ( -- * Basic
   elem, any, eq, to, list,
   span, equal,
   inter, begin, end,
   succ, fail, what,
   change,
 ) where

import qualified Prelude as P
import           Prelude hiding ( elem, any, span, succ, fail )

import qualified Data.List                       as L
import qualified Koshucode.Baala.Subtext.Fn      as S
import qualified Koshucode.Baala.Subtext.Expr    as S

-- | Element match.
elem :: S.Name -> (a -> Bool) -> S.Expr a
elem n f = S.EBase $ S.EElem $ S.Fn n f

-- | Match any element.
any :: S.Expr a
any = elem "any" $ const True

-- | Match exactly the given element.
eq :: (Eq a) => a -> S.Expr a
eq x = elem "eq" (== x)

-- | Match element range.
to :: (Ord a) => a -> a -> S.Expr a
to low up = elem "to" f where
    f x = x >= low && x <= up

-- | Match element in the given list.
list :: (Eq a) => [a] -> S.Expr a
list xs = elem "list" (`P.elem` xs)

-- | Spanning match.
span :: S.Name -> ([a] -> Maybe ([a], [a])) -> S.Expr a
span n f = S.EBase $ S.ESpan $ S.Fn n f

-- | Match exactly the given sequence.
equal :: (Eq a) => [a] -> S.Expr a
equal w = span "equal" f where
    f s = do s' <- L.stripPrefix w s
             Just (reverse w, s')

-- | Inter-element match.
inter :: S.Name -> (Maybe a -> Maybe a -> Bool) -> S.Expr a
inter n f = S.EBase $ S.EInter $ S.Fn2 n f

-- | Match at the beginning of sequence.
begin :: S.Expr a
begin = inter "begin" f where
    f Nothing _  = True
    f _ _        = False

-- | Match at the end of sequence.
end :: S.Expr a
end = inter "end" f where
    f _ Nothing  = True
    f _ _        = False

-- | Match immediately.
succ :: S.Expr a
succ = always True

-- | Unmatch immediately.
fail :: S.Expr a
fail = always False

always :: Bool -> S.Expr a
always = S.EBase . S.EAlways

-- | Context-dependent match.
what :: S.Expr a
what = S.EBase S.EWhat

-- | Change to other matcher.
change :: S.Name -> S.Expr a
change n = S.EBase $ S.EChange n

