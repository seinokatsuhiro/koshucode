{-# OPTIONS_GHC -Wall #-}

-- | Subtree.

module Koshucode.Baala.Syntax.Subtree.Subtree
  ( -- * Pattern
    SubtreePattern (..),
    subtreeL,
    SubtreeTerm (..),
    subtreeText, subtreeTexts,

    -- * Conversion
    Subtree,
    SubtreeOutput,
    subtree, subtreeWith,
    subtreeOne,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Symbol           as S
import qualified Koshucode.Baala.Syntax.Subtree.Filter   as S


-- ============================================  Pattern

-- | Subtree pattern.
data SubtreePattern
    = SubtreeL SubtreeTerm S.SubtreeFilter       -- ^ Leaf
    | SubtreeB S.SubtreeFilter [SubtreePattern]  -- ^ Branch
    | SubtreeR S.SubtreeFilter [SubtreePattern]  -- ^ Recursive branch
      deriving (Show, Eq)

-- | Create non-termification leaf pattern.
subtreeL :: S.SubtreeFilter -> SubtreePattern
subtreeL = SubtreeL SubtreeNone

-- | Termification of subtree content.
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


-- ============================================  Conversion

-- | Subtree.
type Subtree a = B.RawTree [SubtreePattern] a a

-- | Subtree with termified elements.
type SubtreeOutput a = B.RawTree [SubtreePattern] a (SubtreeTerm, a)

-- | Select subtree.
--
--   >>> let tree = B.TreeB [] "Y1" [B.TreeL "Z1", B.TreeL "Z2", B.TreeB [] "Y2" [B.TreeL "Z3"]]
--   >>> B.printTree tree
--   > [] "Y1"
--     - "Z1"
--     - "Z2"
--     > [] "Y2"
--       - "Z3"
--
--   >>> B.printTrees $ subtree [SubtreeB (S.subtreeEq "Y1") [subtreeL (S.subtreeEq "Z1")]] [tree]
--   > [] "Y1"
--     - (SubtreeNone,"Z1")
--
--   >>> B.printTrees $ subtree [SubtreeR (S.subtreeId) [subtreeL (S.subtreeEq "Z3")]] [tree]
--   > [] "Y1"
--     > [] "Y2"
--       - (SubtreeNone,"Z3")
--
--   >>> B.printTrees $ subtree [SubtreeR (S.subtreeId) [subtreeL (S.subtreeEq "Z1")]] [tree]
--   > [] "Y1"
--     - (SubtreeNone,"Z1")
--
--   >>> B.printTrees $ subtree [SubtreeB (S.subtreeId) [SubtreeL (subtreeText "A" "/z") (S.subtreeEq "Z1")]] [tree]
--   > [] "Y1"
--     - (SubtreeText ["A"] (TermName EQ "z"),"Z1")
--
subtree :: [SubtreePattern] -> [Subtree String] -> [SubtreeOutput String]
subtree = subtreeWith id

-- | Select subtree with string getter.
subtreeWith :: (a -> String) -> [SubtreePattern] -> [Subtree a] -> [SubtreeOutput a]
subtreeWith get = loop where
    loop ps0 ts = p1 O.<?> ts where
        p1 t = maybeHead (p2 t O.<?> ps0)

        p2 t@(B.TreeL z) (SubtreeL term f)
            | nullZ get f t  = Nothing
            | otherwise      = Just $ B.TreeL (term, z)
        p2 t@(B.TreeB _ y xs) (SubtreeB f ps)
            | nullY get f t  = Nothing
            | otherwise      = Just $ B.TreeB [] y (loop ps xs)
        p2 t@(B.TreeB _ y xs) (SubtreeR f ps)
            | nullY get f t  = Nothing
            | otherwise      = case loop (SubtreeR f ps : ps) xs of
                                 []  -> Nothing
                                 xs' -> Just $ B.TreeB [] y xs'
        p2 _ _ = Nothing

-- | Select subtree.
subtreeOne :: [SubtreePattern] -> O.Map [Subtree String]
subtreeOne ps0 ts = p1 O.<?> ts where
    p1 t = maybeHead (p2 t O.<?> ps0)

    p2 t@(B.TreeL _) (SubtreeL _ f)
        | nullZ id f t  = Nothing
        | otherwise     = Just t
    p2 t@(B.TreeB _ y xs) (SubtreeB f ps)
        | nullY id f t  = Nothing
        | otherwise     = Just $ B.TreeB ps y xs
    p2 t@(B.TreeB _ y xs) (SubtreeR f ps)
        | nullY id f t  = Nothing
        | otherwise     = Just $ B.TreeB (SubtreeR f ps : ps) y xs
    p2 _ _ = Nothing

maybeHead :: [a] -> Maybe a
maybeHead []       = Nothing
maybeHead (a : _)  = Just a

nullY :: (a -> String) -> S.SubtreeFilter -> Subtree a -> Bool
nullY get f t = null $ S.subtreeFilterOn (get <.> getTreeY) f [t]

nullZ :: (a -> String) -> S.SubtreeFilter -> Subtree a -> Bool
nullZ get f t = null $ S.subtreeFilterOn (get <.> getTreeZ) f [t]

-- | 'fmap' composition.
(<.>) :: (Functor f) => (b -> c) -> (a -> f b) -> a -> f c
(<.>) g f x = g <$> f x

-- | Branch element of tree.
getTreeY :: B.RawTree b y z -> Maybe y
getTreeY (B.TreeB _ y _)  = Just y
getTreeY _                = Nothing

-- | Leaf element of tree.
getTreeZ :: B.RawTree b y z -> Maybe z
getTreeZ (B.TreeL z)  = Just z
getTreeZ _            = Nothing
