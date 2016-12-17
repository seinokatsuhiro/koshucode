{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parser for token tree.

module Koshucode.Baala.Syntax.TTree.Parse
  ( -- * Type
    Tree, TTree, NamedTrees,

    -- * Parser
    ToTrees (..),
    ttreeGroup,
    withTrees,
    readClauseTrees,

    -- * Pretty print
    treesDoc, ppTrees,
  ) where

import qualified Text.PrettyPrint                        as P
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.TTree.Bracket    as S


-- --------------------------------------------  Type

-- | Tree of tokens.
type Tree = B.CodeTree S.BracketType S.Token

-- | Tree of tokens.
{-# DEPRECATED TTree "Use 'Tree' instead." #-}
type TTree = Tree

-- | Pair of token trees and its name.
type NamedTrees = B.Named [Tree]

-- | Wrap trees in group.
ttreeGroup :: [Tree] -> Tree
ttreeGroup = B.treeWrap S.BracketGroup

-- --------------------------------------------  Parser

-- | Convert to token trees.
class ToTrees a where
    -- | Convert to list of token trees.
    toTrees :: a -> B.Ab [Tree]

    -- | Convert to token tree.
    toTree :: a -> B.Ab Tree
    toTree a = ttreeGroup <$> toTrees a

    -- | List of token trees or error.
    toTrees' :: a -> [Tree]
    toTrees' = B.unabort . toTrees

    -- | Token tree or error.
    toTree' :: a -> Tree
    toTree' = B.unabort . toTree

instance ToTrees String where
    toTrees = tokenTrees . S.toks

instance ToTrees [S.Token] where
    toTrees = tokenTrees

instance ToTrees [Tree] where
    toTrees = Right

-- | Parse tokens with brackets into trees.
--   Blank tokens and comments are excluded.
tokenTrees :: [S.Token] -> B.Ab [Tree]
tokenTrees = B.trees S.getBracketType B.BracketNone . S.prepareTokens

-- | Call function with token trees.
--
--   >>> withTrees id "a"
--   Right [TreeL (TText /0.1.0/ TextRaw "a")]
--
withTrees :: (ToTrees a) => ([Tree] -> b) -> a -> B.Ab b
withTrees f a =
    do ts <- toTrees a
       Right $ f ts

-- | Read clauses and convert to token trees.
readClauseTrees :: FilePath -> B.IOAb [[Tree]]
readClauseTrees path =
    do toks' <- S.readClauseTokens path
       return $ case toks' of
         Right toks -> mapM toTrees toks
         Left a     -> Left a

-- --------------------------------------------  Pretty print

-- | Convert token trees to 'B.Doc' value for pretty printing.
treesDoc :: [Tree] -> B.Doc
treesDoc = dv where
    dv = B.pprintV . map d
    d (B.TreeL x) = B.pprint "|" B.<+> B.pprint x
    d (B.TreeB br oc xs) =
        let tag   = "<" ++ B.name br ++ ">"
            treeB = B.pprint tag B.<+> brackets oc
        in P.hang treeB 2 $ dv xs

    brackets Nothing = B.pprint ""
    brackets (Just (open, close)) =
        B.pprintH [B.pprint open, B.pprint "--", B.pprint close]

-- | Pretty print token trees.
--
--   >>> ppTrees "a ( b c )"
--   | TText /1.0/ TextRaw "a"
--   <group> TOpen /1.2/ "(" -- TClose /1.8/ ")"
--     | TText /1.4/ TextRaw "b"
--     | TText /1.6/ TextRaw "c"
--
ppTrees :: (ToTrees a) => a -> IO ()
ppTrees a = case toTrees a of
              Left ab   -> putStrLn `mapM_` B.abortMessage [] ab
              Right ts  -> print $ treesDoc ts
