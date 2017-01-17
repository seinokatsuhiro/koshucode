{-# OPTIONS_GHC -Wall #-}

-- | Subtree filter.

module Koshucode.Baala.Syntax.Subtree.Filter
  ( Sivmap (..),
    sivmapId, sivmapEq,
    sivmapKeep, sivmapOmit,
    sivmapAssoc,
    sivmapChain,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Subtext                 as S

-- | Subtree filter.
data Sivmap
    = SivmapId                         -- ^ Identity
    | SivmapEq    String               -- ^ Equality
    | SivmapKeep  String S.SivExpr     -- ^ Keep by sieve pattern
    | SivmapOmit  String S.SivExpr     -- ^ Omit by sieve pattern
    | SivmapAssoc String String        -- ^ Associatoin of key and value
    | SivmapChain Sivmap Sivmap        -- ^ Filter chain
      deriving (Show, Eq)

instance Monoid Sivmap where
    mempty  = SivmapId
    mappend = sivmapChain

-- | Identity filter.
sivmapId :: Sivmap
sivmapId = SivmapId

-- | Equal filter.
sivmapEq :: String -> Sivmap
sivmapEq = SivmapEq

-- | Filter by sieve pattern.
sivmapKeep :: String -> Sivmap
sivmapKeep s = SivmapKeep s (S.toSivExprOr S.fail s)

-- | Anti-filter by sieve pattern.
sivmapOmit :: String -> Sivmap
sivmapOmit s = SivmapOmit s (S.toSivExprOr S.fail s)

-- | Filter chain.
sivmapChain :: O.Bin Sivmap
sivmapChain SivmapId f = f
sivmapChain f SivmapId = f
sivmapChain f g = SivmapChain f g

-- | Association filter.
sivmapAssoc :: String -> String -> Sivmap
sivmapAssoc = SivmapAssoc
