{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Subtree.

module Koshucode.Baala.Syntax.Subtree.Subtree
  ( -- * Pattern
    SubtreePattern (..),
    subtreeL,
    subtreeB,
    SubtreeTerm (..),
    subtreeText, subtreeTexts,

    -- * Conversion
    Subtree,
    SubtreeOutput,
    SubtreeValue,
    subtreeStringTest,
    subtree, subtreeBy,
    subtreeOne,
    subtreeData,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Subtext                 as U
import qualified Koshucode.Baala.Syntax.Symbol           as S
import qualified Koshucode.Baala.Syntax.Subtree.Filter   as S


-- ============================================  Pattern

-- | Subtree pattern.
data SubtreePattern
    = SubtreeL SubtreeTerm S.SubtreeFilter       -- ^ Leaf
    | SubtreeB SubtreeTerm S.SubtreeFilter [SubtreePattern]  -- ^ Branch
    | SubtreeR S.SubtreeFilter [SubtreePattern]  -- ^ Recursive branch
      deriving (Show, Eq)

-- | Create non-termification leaf pattern.
subtreeL :: S.SubtreeFilter -> SubtreePattern
subtreeL = SubtreeL SubtreeNone

-- | Create non-termification branch pattern.
subtreeB :: S.SubtreeFilter -> [SubtreePattern] -> SubtreePattern
subtreeB = SubtreeB SubtreeNone

-- | Termification of subtree content.
data SubtreeTerm
    = SubtreeNone
    | SubtreeText [S.JudgeClass] S.TermName
    | SubtreeSeq  [S.JudgeClass] S.TermName
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
type SubtreeOutput a = B.RawTree [SubtreePattern] (SubtreeTerm, a) (SubtreeTerm, a)

-- | Subtree with value.
type SubtreeValue = SubtreeOutput O.Value

-- | Test string matches selection rule.
--
--   >>> subtreeStringTest (S.subtreeEq "foo") <$> ["foo", "foobar", "barfoo"]
--   [True,False,False]
--
--   >>> subtreeStringTest (S.subtreeKeep "foo(*)") <$> ["foo", "foobar", "barfoo"]
--   [True,True,False]
--
subtreeStringTest :: S.SubtreeFilter -> String -> Bool
subtreeStringTest (S.SubtreeId)        _  = True
subtreeStringTest (S.SubtreeEq s)      t  = s == t
subtreeStringTest (S.SubtreeKeep _ e)  t  =       U.sivMatchExpr e t
subtreeStringTest (S.SubtreeOmit _ e)  t  = not $ U.sivMatchExpr e t
subtreeStringTest (S.SubtreeChain f g) t  = subtreeStringTest f t && subtreeStringTest g t
subtreeStringTest (S.SubtreeAttr _ _)  _  = False

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
--   >>> B.printTrees $ subtree [subtreeB (S.subtreeEq "Y1") [subtreeL (S.subtreeEq "Z1")]] [tree]
--   > [] (SubtreeNone,"Y1")
--     - (SubtreeNone,"Z1")
--
--   >>> B.printTrees $ subtree [SubtreeR (S.subtreeId) [subtreeL (S.subtreeEq "Z3")]] [tree]
--   > [] (SubtreeNone,"Y1")
--     > [] (SubtreeNone,"Y2")
--       - (SubtreeNone,"Z3")
--
--   >>> B.printTrees $ subtree [SubtreeR (S.subtreeId) [subtreeL (S.subtreeEq "Z1")]] [tree]
--   > [] (SubtreeNone,"Y1")
--     - (SubtreeNone,"Z1")
--
--   >>> B.printTrees $ subtree [SubtreeB (subtreeText "A" "/y") (S.subtreeId) [SubtreeL (subtreeText "A" "/z") (S.subtreeEq "Z1")]] [tree]
--   > [] (SubtreeText ["A"] (TermName EQ "y"),"Y1")
--     - (SubtreeText ["A"] (TermName EQ "z"),"Z1")
--
subtree :: [SubtreePattern] -> [Subtree String] -> [SubtreeOutput String]
subtree = subtreeBy subtreeStringTest

-- | Select subtree by element tester.
subtreeBy :: (S.SubtreeFilter -> a -> Bool) -> [SubtreePattern] -> [Subtree a] -> [SubtreeOutput a]
subtreeBy test = loop where
    loop ps0 ts = p1 O.<++> ts where
        p1 t = p2 t O.<?> ps0

        p2 (B.TreeL z) (SubtreeL term f)
            | test f z     = Just $ B.TreeL (term, z)
            | otherwise    = Nothing
        p2 (B.TreeB _ y xs) (SubtreeB term f ps)
            | test f y     = Just $ B.TreeB [] (term, y) (loop ps xs)
            | otherwise    = Nothing
        p2 (B.TreeB _ y xs) (SubtreeR f ps)
            | test f y     = case loop (SubtreeR f ps : ps) xs of
                               []  -> Nothing
                               xs' -> Just $ B.TreeB [] (SubtreeNone, y) xs'
            | otherwise    = Nothing
        p2 _ _ = Nothing

-- | Select subtree.
subtreeOne :: [SubtreePattern] -> O.Map [Subtree String]
subtreeOne = subtreeOneBy subtreeStringTest

subtreeOneBy :: (S.SubtreeFilter -> String -> Bool) -> [SubtreePattern] -> O.Map [Subtree String]
subtreeOneBy test ps0 ts = p1 O.<++> ts where
    p1 t = p2 t O.<?> ps0

    p2 t@(B.TreeL z) (SubtreeL _ f)
        | test f z      = Just t
        | otherwise     = Nothing
    p2 (B.TreeB _ y xs) (SubtreeB _ f ps)
        | test f y      = Just $ B.TreeB ps y xs
        | otherwise     = Nothing
    p2 (B.TreeB _ y xs) (SubtreeR f ps)
        | test f y      = Just $ B.TreeB (SubtreeR f ps : ps) y xs
        | otherwise     = Nothing
    p2 _ _ = Nothing

-- | Extract data from subtree.
subtreeData :: SubtreeValue -> [S.RawJudge O.Value]
subtreeData tree = gatherData O.<++> subtreeTerms tree

gatherData :: [(S.JudgeClass, S.Term O.Value)] -> [S.RawJudge O.Value]
gatherData xs = gatherTerms O.<$$> B.gatherToAssocOrder xs

gatherTerms :: O.Map [(S.Term O.Value)]
gatherTerms ts = content O.<$$> B.gatherToAssocOrder ts where
    content [c] = c
    content cs  = O.VList cs

subtreeTerms :: SubtreeValue -> [[(S.JudgeClass, S.Term O.Value)]]
subtreeTerms = branch where
    branch t@(B.TreeL _) = [leaf t]
    branch (B.TreeB _ term xs) =
        case B.partitionLB xs of
          (ls, bs) -> let ts = val term ++ (leaf O.<++> ls)
                      in if null bs
                         then [ts]
                         else (ts ++) <$> (branch O.<++> bs)

    leaf (B.TreeL term)   = val term
    leaf (B.TreeB _ _ _)  = []

    val (SubtreeText cs n, v)  = (, (n, v)) <$> cs
    val (SubtreeSeq  cs n, v)  = (, (n, v)) <$> cs
    val (SubtreeNone, _)       = []

