{-# OPTIONS_GHC -Wall #-}

-- | Subtext match expression.

module Koshucode.Baala.Subtext.Expr
 ( Expr (..),
   ExprRec (..),
   ExprBase (..),
   MinMax (..),
   FnElem, FnSpan, FnInter,
 ) where

import qualified Koshucode.Baala.Subtext.Fn  as S


-- | Subtext match expression.
data Expr a
  = ERec  (ExprRec a)          -- ^ Recursive expression
  | EBase (ExprBase a)         -- ^ Non-recursive expression
    deriving (Show, Eq, Ord)

-- | Recursive expression.
data ExprRec a
  = EOr           [Expr a]     -- ^ Alternative match
  | ESeq          [Expr a]     -- ^ Sequential match
  | ENot (Expr a) [Expr a]     -- ^ Match with exceptions
  | ERep  MinMax  (Expr a)     -- ^ Repetitive match
  | ESub  S.Name  (Expr a)     -- ^ Submatch
  | EGath Bool    (Expr a)     -- ^ Change gathering setting
  | EPeek         (Expr a)     -- ^ Match but not consume input
    deriving (Show, Eq, Ord)

-- | Non-recursive expression.
data ExprBase a
  = EElem  (FnElem a)          -- ^ Element matcher
  | ESpan  (FnSpan a)          -- ^ Spanning (multi-element) matcher
  | EInter (FnInter a)         -- ^ Inter-element matcher
  | EChange S.Name             -- ^ Change to other expression
  | EAlways Bool               -- ^ Immediate match/unmatch
  | EWhat                      -- ^ Context-dependent match
    deriving (Show, Eq, Ord)

-- | Lower and upper bound.
data MinMax
  = Min    Int            -- ^ Lower bound
  | MinMax Int Int        -- ^ Lower and upper bound
    deriving (Show, Eq, Ord)

-- | Function type for element matcher.
type FnElem a = S.Fn a Bool

-- | Function type for spanning matcher.
type FnSpan a = S.Fn [a] (Maybe ([a], [a]))

-- | Function type for inter-element matcher.
type FnInter a = S.Fn2 (Maybe a) (Maybe a) Bool

