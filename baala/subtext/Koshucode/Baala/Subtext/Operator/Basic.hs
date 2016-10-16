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
import qualified Koshucode.Baala.Overture.Fn     as O
import qualified Koshucode.Baala.Subtext.Expr    as T

-- | Element match.
elem :: O.Name -> (a -> Bool) -> T.Expr a
elem n f = T.EBase $ T.EElem $ O.Fn n f

-- | Match any element.
any :: T.Expr a
any = elem "any" $ const True

-- | Match exactly the given element.
eq :: (Eq a) => a -> T.Expr a
eq x = elem "eq" (== x)

-- | Match element range.
to :: (Ord a) => a -> a -> T.Expr a
to low up = elem "to" f where
    f x = x >= low && x <= up

-- | Match element in the given list.
list :: (Eq a) => [a] -> T.Expr a
list xs = elem "list" (`P.elem` xs)

-- | Spanning match.
span :: O.Name -> ([a] -> Maybe ([a], [a])) -> T.Expr a
span n f = T.EBase $ T.ESpan $ O.Fn n f

-- | Match exactly the given sequence.
equal :: (Eq a) => [a] -> T.Expr a
equal w = span "equal" f where
    f s = do s' <- L.stripPrefix w s
             Just (reverse w, s')

-- | Inter-element match.
inter :: O.Name -> (Maybe a -> Maybe a -> Bool) -> T.Expr a
inter n f = T.EBase $ T.EInter $ O.Fn2 n f

-- | Match at the beginning of sequence.
begin :: T.Expr a
begin = inter "begin" f where
    f Nothing _  = True
    f _ _        = False

-- | Match at the end of sequence.
end :: T.Expr a
end = inter "end" f where
    f _ Nothing  = True
    f _ _        = False

-- | Match immediately.
succ :: T.Expr a
succ = always True

-- | Unmatch immediately.
fail :: T.Expr a
fail = always False

always :: Bool -> T.Expr a
always = T.EBase . T.EAlways

-- | Context-dependent match.
what :: T.Expr a
what = T.EBase T.EWhat

-- | Change to other matcher.
change :: O.Name -> T.Expr a
change n = T.EBase $ T.EChange n

