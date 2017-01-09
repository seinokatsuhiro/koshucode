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

import qualified System.FilePath.Glob                    as Glob
import qualified Koshucode.Baala.Overture                as O

-- | Subtree filter.
data SubtreeFilter
    = SubtreeId                                 -- ^ Identity
    | SubtreeEq    String                       -- ^ Equality
    | SubtreeKeep  String Glob.Pattern          -- ^ Glob
    | SubtreeOmit  String Glob.Pattern          -- ^ Anti-glob
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

-- | Glob filter.
subtreeKeep :: String -> SubtreeFilter
subtreeKeep s = SubtreeKeep s $ Glob.compile s

-- | Anti-glob filter.
subtreeOmit :: String -> SubtreeFilter
subtreeOmit s = SubtreeOmit s $ Glob.compile s

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
    a xs (SubtreeKeep _ p : fs)    = a xs' fs where xs' = O.keepOn get (Glob.match p) xs
    a xs (SubtreeOmit _ p : fs)    = a xs' fs where xs' = O.omitOn get (Glob.match p) xs
    a xs (SubtreeAttr _ _ : fs)    = a xs fs
    a xs (SubtreeChain f1 f2 : fs) = a xs fs' where fs' = f1 : f2 : fs

