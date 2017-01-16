{-# OPTIONS_GHC -Wall #-}

-- | Subtree filter.

module Koshucode.Baala.Syntax.Subtree.Filter
  ( SubtreeFilter (..),
    subtreeId, subtreeEq,
    subtreeKeep, subtreeOmit,
    subtreeChain,
    subtreeAttr,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Subtext                 as S

-- | Subtree filter.
data SubtreeFilter
    = SubtreeId                                 -- ^ Identity
    | SubtreeEq    String                       -- ^ Equality
    | SubtreeKeep  String S.SivExpr             -- ^ Keep by sieve pattern
    | SubtreeOmit  String S.SivExpr             -- ^ Omit by sieve pattern
    | SubtreeAttr  String String                -- ^ Attribute
    | SubtreeChain SubtreeFilter SubtreeFilter  -- ^ Filter chain
      deriving (Show, Eq)

instance Monoid SubtreeFilter where
    mempty  = SubtreeId
    mappend = subtreeChain

-- | Identity filter.
subtreeId :: SubtreeFilter
subtreeId = SubtreeId

-- | Equal filter.
subtreeEq :: String -> SubtreeFilter
subtreeEq = SubtreeEq

-- | Filter by sieve pattern.
subtreeKeep :: String -> SubtreeFilter
subtreeKeep s = SubtreeKeep s (S.toSivExprOr S.fail s)

-- | Anti-filter by sieve pattern.
subtreeOmit :: String -> SubtreeFilter
subtreeOmit s = SubtreeOmit s (S.toSivExprOr S.fail s)

-- | Filter chain.
subtreeChain :: O.Bin SubtreeFilter
subtreeChain SubtreeId f = f
subtreeChain f SubtreeId = f
subtreeChain f g = SubtreeChain f g

-- | Attribute filter.
subtreeAttr :: String -> String -> SubtreeFilter
subtreeAttr = SubtreeAttr
