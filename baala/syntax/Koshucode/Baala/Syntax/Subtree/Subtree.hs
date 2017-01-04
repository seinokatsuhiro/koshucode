{-# OPTIONS_GHC -Wall #-}

-- | Subtree.

module Koshucode.Baala.Syntax.Subtree.Subtree
  ( -- * Pattern
    Subtree,
    SubtreeOutput,
    SubtreePattern (..),
    subtreeL,
    SubtreeTerm (..),
    subtreeText, subtreeTexts,
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
import qualified Koshucode.Baala.Syntax.Symbol           as S


-- ============================================  Pattern

-- | Subtree.
type Subtree = B.RawTree [SubtreePattern] String String

-- | Subtree output.
type SubtreeOutput = B.RawTree [SubtreePattern] String (SubtreeTerm, String)

-- | Subtree pattern.
data SubtreePattern
    = SubtreeL SubtreeTerm SubtreeFilter       -- ^ Leaf
    | SubtreeB SubtreeFilter [SubtreePattern]  -- ^ Branch
    | SubtreeR SubtreeFilter [SubtreePattern]  -- ^ Recursive branch
      deriving (Show, Eq)

-- | Create non-termination leaf pattern.
subtreeL :: SubtreeFilter -> SubtreePattern
subtreeL = SubtreeL SubtreeNone

-- | Termination of subtree content.
data SubtreeTerm
    = SubtreeNone
    | SubtreeText [String] S.TermName
    | SubtreeSeq  S.TermName
      deriving (Show, Eq)

-- | Create text term.
subtreeText :: (S.ToTermName n) => String -> n -> SubtreeTerm
subtreeText c = subtreeTexts [c]

-- | Create text term.
subtreeTexts :: (S.ToTermName n) => [String] -> n -> SubtreeTerm
subtreeTexts cs n = SubtreeText cs $ S.toTermName n

-- | Select subtree.
--
--   >>> let tree = B.TreeB [] "Y1" [B.TreeL "Z1", B.TreeL "Z2", B.TreeB [] "Y2" [B.TreeL "Z3"]]
--   >>> O.putLines $ B.ppRawTree tree
--   > [] "Y1"
--     - "Z1"
--     - "Z2"
--     > [] "Y2"
--       - "Z3"
--
--   >>> O.putLines (B.ppRawTree O.<++> subtree [SubtreeB (subtreeEq "Y1") [subtreeL (subtreeEq "Z1")]] [tree])
--   > [] "Y1"
--     - (SubtreeNone,"Z1")
--
--   >>> O.putLines (B.ppRawTree O.<++> subtree [SubtreeR (subtreeId) [subtreeL (subtreeEq "Z3")]] [tree])
--   > [] "Y1"
--     > [] "Y2"
--       - (SubtreeNone,"Z3")
--
--   >>> O.putLines (B.ppRawTree O.<++> subtree [SubtreeR (subtreeId) [subtreeL (subtreeEq "Z1")]] [tree])
--   > [] "Y1"
--     - (SubtreeNone,"Z1")
--
--   >>> O.putLines (B.ppRawTree O.<++> subtree [SubtreeB (subtreeId) [SubtreeL (subtreeText "A" "/z") (subtreeEq "Z1")]] [tree])
--   > [] "Y1"
--     - (SubtreeText ["A"] (TermName EQ "z"),"Z1")
--
subtree :: [SubtreePattern] -> [Subtree] -> [SubtreeOutput]
subtree ps0 ts = p1 O.<?> ts where
    p1 t = maybeHead (p2 t O.<?> ps0)

    p2 t@(B.TreeL z) (SubtreeL term f)
        | nullZ f t  = Nothing
        | otherwise  = Just $ B.TreeL (term, z)
    p2 t@(B.TreeB _ y xs) (SubtreeB f ps)
        | nullY f t  = Nothing
        | otherwise  = Just $ B.TreeB [] y (subtree ps xs)
    p2 t@(B.TreeB _ y xs) (SubtreeR f ps)
        | nullY f t  = Nothing
        | otherwise  = case subtree (SubtreeR f ps : ps) xs of
                         []  -> Nothing
                         xs' -> Just $ B.TreeB [] y xs'
    p2 _ _ = Nothing

-- | Select subtree.
subtreeOne :: [SubtreePattern] -> O.Map [Subtree]
subtreeOne ps0 ts = p1 O.<?> ts where
    p1 t = maybeHead (p2 t O.<?> ps0)

    p2 t@(B.TreeL _) (SubtreeL _ f)
        | nullZ f t  = Nothing
        | otherwise  = Just t
    p2 t@(B.TreeB _ y xs) (SubtreeB f ps)
        | nullY f t  = Nothing
        | otherwise  = Just $ B.TreeB ps y xs
    p2 t@(B.TreeB _ y xs) (SubtreeR f ps)
        | nullY f t  = Nothing
        | otherwise  = Just $ B.TreeB (SubtreeR f ps : ps) y xs
    p2 _ _ = Nothing

nullZ :: SubtreeFilter -> Subtree -> Bool
nullZ f t = null $ subtreeFilterOn getTreeZ f [t]

nullY :: SubtreeFilter -> Subtree -> Bool
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


-- ============================================  Filter

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

