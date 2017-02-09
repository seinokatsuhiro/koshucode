{-# OPTIONS_GHC -Wall #-}

-- | Subtext match expression.

module Koshucode.Baala.Subtext.Expr
 ( Expr (..),
   ExprRec (..),
   ExprBase (..),
   FnElem, FnSpan, FnInter, FnAs,
 ) where

import qualified Koshucode.Baala.Overture.Fn     as O
import qualified Koshucode.Baala.Subtext.MinMax  as T


-- | Subtext match expression.
data Expr as a
  = ERec  (ExprRec as a)      -- ^ Recursive expression
  | EBase (ExprBase as a)     -- ^ Non-recursive expression
    deriving (Show, Eq, Ord)

-- | Recursive expression.
data ExprRec as a
  = EOr              [Expr as a]   -- ^ Alternative match
  | ESeq             [Expr as a]   -- ^ Sequential match
  | EAnd             [Expr as a]   -- ^ Additional condition
  | ENot             (Expr as a)   -- ^ Inverted condition
  | ERep T.MinMax    (Expr as a)   -- ^ Repetitive match
  | ELast            (Expr as a)   -- ^ Find last match
  | ESub  O.Name     (Expr as a)   -- ^ Submatch
  | EAs (FnAs as a)  (Expr as a)   -- ^ Modification matcher
  | EGath Bool       (Expr as a)   -- ^ Change gathering setting
    deriving (Show, Eq, Ord)

-- | Non-recursive expression.
data ExprBase as a
  = EElem  (FnElem a)          -- ^ Element matcher
  | ESpan  (FnSpan as a)       -- ^ Spanning (multi-element) matcher
  | EInter (FnInter a)         -- ^ Inter-element matcher
  | EChange O.Name             -- ^ Change to other expression
  | EAlways Bool               -- ^ Immediate match/unmatch
  | EWhat                      -- ^ Context-dependent match
    deriving (Show, Eq, Ord)

-- | Function type for element matcher.
type FnElem a = O.Fn a Bool

-- | Function type for spanning matcher.
type FnSpan as a = O.Fn as (Maybe (as, as))

-- | Function type for inter-element matcher.
type FnInter a = O.Fn2 (Maybe a) (Maybe a) Bool

-- | Function type for modification matcher.
type FnAs as a = O.Fn as as

