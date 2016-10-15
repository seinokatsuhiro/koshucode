{-# OPTIONS_GHC -Wall #-}

-- | Subtext match expression.

module Koshucode.Baala.Subtext.Expr
 ( Expr (..),
   ExprRec (..),
   ExprBase (..),
   FnElem, FnSpan, FnInter, FnAs,
 ) where

import qualified Koshucode.Baala.Overture.Fn     as O
import qualified Koshucode.Baala.Subtext.MinMax  as S


-- | Subtext match expression.
data Expr a
  = ERec  (ExprRec a)          -- ^ Recursive expression
  | EBase (ExprBase a)         -- ^ Non-recursive expression
    deriving (Show, Eq, Ord)

-- | Recursive expression.
data ExprRec a
  = EOr           [Expr a]     -- ^ Alternative match
  | ESeq          [Expr a]     -- ^ Sequential match
  | EAnd          [Expr a]     -- ^ Additional condition
  | ENot          (Expr a)     -- ^ Inverted condition
  | ERep S.MinMax (Expr a)     -- ^ Repetitive match
  | ELast         (Expr a)     -- ^ Find last match
  | ESub  O.Name  (Expr a)     -- ^ Submatch
  | EAs (FnAs a)  (Expr a)     -- ^ Modification matcher
  | EGath Bool    (Expr a)     -- ^ Change gathering setting
    deriving (Show, Eq, Ord)

-- | Non-recursive expression.
data ExprBase a
  = EElem  (FnElem a)          -- ^ Element matcher
  | ESpan  (FnSpan a)          -- ^ Spanning (multi-element) matcher
  | EInter (FnInter a)         -- ^ Inter-element matcher
  | EChange O.Name             -- ^ Change to other expression
  | EAlways Bool               -- ^ Immediate match/unmatch
  | EWhat                      -- ^ Context-dependent match
    deriving (Show, Eq, Ord)

-- | Function type for element matcher.
type FnElem a = O.Fn a Bool

-- | Function type for spanning matcher.
type FnSpan a = O.Fn [a] (Maybe ([a], [a]))

-- | Function type for inter-element matcher.
type FnInter a = O.Fn2 (Maybe a) (Maybe a) Bool

-- | Function type for modification matcher.
type FnAs a = O.Fn [a] [a]

