{-# OPTIONS_GHC -Wall #-}

-- | Subtree.

module Koshucode.Baala.Syntax.Subtree.Subtree
  ( -- * Pattern
    Subtree,
    SubtreePattern (..),
    subtree, subtreeOne,

    -- * Filter
    SubtreeFilter (..),
    subtreeId, subtreeEq,
    subtreeKeep, subtreeOmit,
    subtreeChain,
    subtreeFilter,
  ) where

import qualified System.FilePath.Glob                    as Glob
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B


-- | Subtree.
type Subtree = B.RawTree [SubtreePattern] String String

-- | Subtree pattern.
data SubtreePattern
    = SubtreeL SubtreeFilter                   -- ^ Leaf
    | SubtreeB SubtreeFilter [SubtreePattern]  -- ^ Branch
    | SubtreeR SubtreeFilter [SubtreePattern]  -- ^ Recursive branch
      deriving (Show, Eq)

-- | Select subtree.
--
--   >>> let tree = B.TreeB [] "Y1" [B.TreeL "Z1", B.TreeL "Z2", B.TreeB [] "Y2" [B.TreeL "Z3"]]
--   >>> O.putLines $ B.ppTree tree
--   > [] "Y1"
--     - "Z1"
--     - "Z2"
--     > [] "Y2"
--       - "Z3"
--
--   >>> O.putLines (B.ppTree O.<++> subtree [SubtreeB (subtreeEq "Y1") [SubtreeL (subtreeEq "Z1")]] [tree])
--   > [] "Y1"
--     - "Z1"
--
--   >>> O.putLines (B.ppTree O.<++> subtree [SubtreeR (subtreeId) [SubtreeL (subtreeEq "Z3")]] [tree])
--   > [] "Y1"
--     > [] "Y2"
--       - "Z3"
--
--   >>> O.putLines (B.ppTree O.<++> subtree [SubtreeR (subtreeId) [SubtreeL (subtreeEq "Z1")]] [tree])
--   > [] "Y1"
--     - "Z1"
--
subtree :: [SubtreePattern] -> O.Map [Subtree]
subtree ps ts = subtreeRec $ subtreeOne ps ts

subtreeRec :: O.Map [Subtree]
subtreeRec ts = p O.<?> ts where
    p t@(B.TreeL _) = Just t
    p (B.TreeB ps y zs) =
        case subtreeRec $ subtreeOne ps zs of
          []  -> Nothing
          zs' -> Just $ B.TreeB [] y zs'

-- | Select subtree.
subtreeOne :: [SubtreePattern] -> O.Map [Subtree]
subtreeOne ps0 ts = p1 O.<?> ts where
    p1 t = maybeHead (p2 t O.<?> ps0)

    p2 t@(B.TreeL _) (SubtreeL f)
        | nullZ f t  = Nothing
        | otherwise  = Just t
    p2 t@(B.TreeB _ y zs) (SubtreeB f ps)
        | nullY f t  = Nothing
        | otherwise  = Just $ B.TreeB ps y zs
    p2 t@(B.TreeB _ y zs) (SubtreeR f ps)
        | nullY f t  = Nothing
        | otherwise  = Just $ B.TreeB (SubtreeR f ps : ps) y zs
    p2 _ _ = Nothing

    nullZ f t = null $ subtreeFilterOn getTreeZ f [t]
    nullY f t = null $ subtreeFilterOn getTreeY f [t]

maybeHead :: [a] -> Maybe a
maybeHead []       = Nothing
maybeHead (a : _)  = Just a

-- | Branch element of tree.
getTreeY :: B.RawTree b y z -> Maybe y
getTreeY (B.TreeB _ y _)  = Just y
getTreeY _                = Nothing

-- | Leaf element of tree.
getTreeZ :: B.RawTree b y z -> Maybe z
getTreeZ (B.TreeL z)  = Just z
getTreeZ _            = Nothing

-- | Subtree filter.
data SubtreeFilter
    = SubtreeId                                 -- ^ Identity
    | SubtreeEq    String                       -- ^ Equality
    | SubtreeKeep  String Glob.Pattern          -- ^ Glob
    | SubtreeOmit  String Glob.Pattern          -- ^ Anti-glob
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
    a xs (SubtreeChain f1 f2 : fs) = a xs fs' where fs' = f1 : f2 : fs

