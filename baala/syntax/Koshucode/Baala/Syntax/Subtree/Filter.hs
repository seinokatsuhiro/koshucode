{-# OPTIONS_GHC -Wall #-}

-- | Subtree filter.

module Koshucode.Baala.Syntax.Subtree.Filter
  ( SubtreeFilter (..),
    subtreeId, subtreeEq,
    subtreeKeep, subtreeOmit,
    subtreeChain,
    subtreeAttr,
    subtreeFilter,
    subtreeFilterOn,
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

-- | Filter strings by subtree filter.
--
--   >>> subtreeFilter subtreeId ["foo", "bar", "foobar"]
--   ["foo","bar","foobar"]
--
--   >>> subtreeFilter (SubtreeEq "bar") ["foo", "bar", "foobar"]
--   ["bar"]
--
--   >>> subtreeFilter (subtreeKeep "foo*") ["foo", "bar", "foobar"]
--   ["foo","foobar"]
--
--   >>> subtreeFilter (subtreeKeep "*bar") ["foo", "bar", "foobar"]
--   ["bar","foobar"]
--
--   >>> subtreeFilter (subtreeOmit "foo*") ["foo", "bar", "foobar"]
--   ["bar"]
--
--   >>> subtreeFilter (subtreeOmit "foo*") ["foo", "bar", "foobar"]
--   ["bar"]
--
--   >>> subtreeFilter (subtreeKeep "foo*" O.++ subtreeOmit "*bar") ["foo", "bar", "foobar"]
--   ["foo"]
--
subtreeFilter :: SubtreeFilter -> O.Map [String]
subtreeFilter = subtreeFilterOn Just

subtreeFilterOn :: (a -> Maybe String) -> SubtreeFilter -> O.Map [a]
subtreeFilterOn get f xs0 = a xs0 [f] where
    a xs []                        = xs
    a xs (SubtreeId : fs)          = a xs fs
    a xs (SubtreeEq x : fs)        = a xs' fs where xs' = O.keepOn get (== x) xs
    a xs (SubtreeKeep _ e : fs)    = a xs' fs where xs' = O.keepOn get (S.sivMatchExpr e) xs
    a xs (SubtreeOmit _ e : fs)    = a xs' fs where xs' = O.omitOn get (S.sivMatchExpr e) xs
    a xs (SubtreeAttr _ _ : fs)    = a xs fs
    a xs (SubtreeChain f1 f2 : fs) = a xs fs' where fs' = f1 : f2 : fs

