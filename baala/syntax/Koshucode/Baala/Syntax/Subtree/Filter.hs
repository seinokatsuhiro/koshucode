{-# OPTIONS_GHC -Wall #-}

-- | Subtree filter.

module Koshucode.Baala.Syntax.Subtree.Filter
  ( Sivmap (..),
    subtreeId, subtreeEq,
    subtreeKeep, subtreeOmit,
    subtreeAssoc,
    subtreeChain,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Subtext                 as S

-- | Subtree filter.
data Sivmap
    = SubtreeId                         -- ^ Identity
    | SubtreeEq    String               -- ^ Equality
    | SubtreeKeep  String S.SivExpr     -- ^ Keep by sieve pattern
    | SubtreeOmit  String S.SivExpr     -- ^ Omit by sieve pattern
    | SubtreeAssoc String String        -- ^ Associatoin of key and value
    | SubtreeChain Sivmap Sivmap        -- ^ Filter chain
      deriving (Show, Eq)

instance Monoid Sivmap where
    mempty  = SubtreeId
    mappend = subtreeChain

-- | Identity filter.
subtreeId :: Sivmap
subtreeId = SubtreeId

-- | Equal filter.
subtreeEq :: String -> Sivmap
subtreeEq = SubtreeEq

-- | Filter by sieve pattern.
subtreeKeep :: String -> Sivmap
subtreeKeep s = SubtreeKeep s (S.toSivExprOr S.fail s)

-- | Anti-filter by sieve pattern.
subtreeOmit :: String -> Sivmap
subtreeOmit s = SubtreeOmit s (S.toSivExprOr S.fail s)

-- | Filter chain.
subtreeChain :: O.Bin Sivmap
subtreeChain SubtreeId f = f
subtreeChain f SubtreeId = f
subtreeChain f g = SubtreeChain f g

-- | Association filter.
subtreeAssoc :: String -> String -> Sivmap
subtreeAssoc = SubtreeAssoc
