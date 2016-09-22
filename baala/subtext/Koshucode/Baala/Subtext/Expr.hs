{-# OPTIONS_GHC -Wall #-}

-- | Subtext match expression.

module Koshucode.Baala.Subtext.Expr
 ( Expr (..),
   ExprRec (..),
   ExprBase (..),
   FnElem, FnSpan, FnInter,
   NameDepth,
   submatchNames,
 ) where

import qualified Data.Map.Strict                 as Map
import qualified Koshucode.Baala.Subtext.Fn      as S
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

-- | Function type for element matcher.
type FnElem a = S.Fn a Bool

-- | Function type for spanning matcher.
type FnSpan a = S.Fn [a] (Maybe ([a], [a]))

-- | Function type for inter-element matcher.
type FnInter a = S.Fn2 (Maybe a) (Maybe a) Bool

-- | Name and depth level.
type NameDepth = (S.Name, Int)

-- | List of submatch names.
submatchNames :: Expr c -> [NameDepth]
submatchNames = Map.assocs . expr 0 where
    expr d (ERec e)    = rec d e
    expr _ _           = Map.empty

    rec d ex = case ex of
                 EOr    es  -> Map.unions (expr d <$> es)
                 ESeq   es  -> Map.unions (expr d <$> es)
                 EAnd   es  -> Map.unions (expr d <$> es)
                 ENot    e  -> expr d e
                 ERep  m e  | S.atMost 1 m  -> expr d e
                            | otherwise     -> expr (d + 1) e
                 ELast   e  -> expr d e
                 ESub  n e  -> Map.insertWith max n d $ expr d e
                 EGath _ e  -> expr d e
                 EPeek   e  -> expr d e

