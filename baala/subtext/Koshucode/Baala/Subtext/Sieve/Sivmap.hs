{-# OPTIONS_GHC -Wall #-}

-- | Textual list mapping.

module Koshucode.Baala.Subtext.Sieve.Sivmap
  ( Sivmap (..),
    sivmapId, sivmapEq,
    sivmapKeep, sivmapOmit,
    sivmapAssoc,
    sivmapChain,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Subtext.Operator        as S
import qualified Koshucode.Baala.Subtext.Sieve.Tree      as S

-- | Mapping for textual lists.
data Sivmap t
    = SivmapId                    -- ^ Identity
    | SivmapEq    t               -- ^ Equality
    | SivmapKeep  t S.SivExpr     -- ^ Keep by sieve pattern
    | SivmapOmit  t S.SivExpr     -- ^ Omit by sieve pattern
    | SivmapAssoc t t             -- ^ Associatoin of key and value
    | SivmapChain (Sivmap t) (Sivmap t)  -- ^ Sivmap chain
      deriving (Show, Eq)

instance Monoid (Sivmap t) where
    mempty  = SivmapId
    mappend = sivmapChain

-- | Identity mapping.
sivmapId :: Sivmap t
sivmapId = SivmapId

-- | Equal mapping.
sivmapEq :: t -> Sivmap t
sivmapEq = SivmapEq

-- | Filter by sieve pattern.
sivmapKeep :: (S.ToSivExpr t) => t -> Sivmap t
sivmapKeep t = SivmapKeep t (S.toSivExprOr S.fail t)

-- | Anti-filter by sieve pattern.
sivmapOmit :: (S.ToSivExpr t) => t -> Sivmap t
sivmapOmit t = SivmapOmit t (S.toSivExprOr S.fail t)

-- | Mapping chain.
sivmapChain :: O.Bin (Sivmap t)
sivmapChain SivmapId f = f
sivmapChain f SivmapId = f
sivmapChain f g = SivmapChain f g

-- | Association mapping.
sivmapAssoc :: t -> t -> Sivmap t
sivmapAssoc = SivmapAssoc
