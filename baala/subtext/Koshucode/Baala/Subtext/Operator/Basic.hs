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

import qualified Koshucode.Baala.Overture        as O
import qualified Koshucode.Baala.Overture.Fn     as O
import qualified Koshucode.Baala.Subtext.Expr    as T

-- | Element match.
elem :: O.Name -> (a -> Bool) -> T.Expr as a
elem n f = T.EBase $ T.EElem $ O.Fn n f

-- | Match any element.
any :: T.Expr as a
any = elem "any" $ const True

-- | Match exactly the given element.
eq :: (Eq a) => a -> T.Expr as a
eq x = elem "eq" (== x)

-- | Match element range.
to :: (Ord a) => a -> a -> T.Expr as a
to low up = elem "to" f where
    f x = x >= low && x <= up

-- | Match element in the given list.
list :: (Eq a) => [a] -> T.Expr as a
list xs = elem "list" (`P.elem` xs)

-- | Spanning match.
span :: O.Name -> (as -> Maybe (as, as)) -> T.Expr as a
span n f = T.EBase $ T.ESpan $ O.Fn n f

-- | Match exactly the given sequence.
equal :: (O.List as a, Eq a) => as -> T.Expr as a
equal w = span "equal" f where
    f s = do s' <- O.stripPrefix w s
             Just (O.reverse w, s')

-- | Inter-element match.
inter :: O.Name -> (Maybe a -> Maybe a -> Bool) -> T.Expr as a
inter n f = T.EBase $ T.EInter $ O.Fn2 n f

-- | Match at the beginning of sequence.
begin :: T.Expr as a
begin = inter "begin" f where
    f Nothing _  = True
    f _ _        = False

-- | Match at the end of sequence.
end :: T.Expr as a
end = inter "end" f where
    f _ Nothing  = True
    f _ _        = False

-- | Match immediately.
succ :: T.Expr as a
succ = always True

-- | Unmatch immediately.
fail :: T.Expr as a
fail = always False

always :: Bool -> T.Expr as a
always = T.EBase . T.EAlways

-- | Context-dependent match.
what :: T.Expr as a
what = T.EBase T.EWhat

-- | Change to other matcher.
change :: O.Name -> T.Expr as a
change n = T.EBase $ T.EChange n

