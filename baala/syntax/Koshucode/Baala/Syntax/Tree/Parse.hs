{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parser for token tree.

module Koshucode.Baala.Syntax.Tree.Parse
  ( -- * Type
    Tree, TTree, NamedTrees,

    -- * Parser
    ToTrees (..),
    ttreeGroup,
    withTrees,
    readClauseTrees,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.Tree.Bracket     as S


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
ttreeGroup = B.codeTreeWrap S.BracketGroup

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

instance ToTrees B.Tx where
    toTrees = toTrees . B.txString

instance ToTrees B.Tz where
    toTrees = toTrees . B.tzString

instance ToTrees B.Bz where
    toTrees = toTrees . B.bzString

instance ToTrees B.Bs where
    toTrees = toTrees . B.bsString

instance ToTrees [S.TToken String] where
    toTrees = tokenTrees

instance ToTrees [Tree] where
    toTrees = Right

instance ToTrees (S.TokenClause String) where
    toTrees = toTrees . B.clauseTokens

-- | Parse tokens with brackets into trees.
--   Blank tokens and comments are excluded.
tokenTrees :: [S.Token] -> B.Ab [Tree]
tokenTrees = B.codeTrees S.getBracketType B.BracketNone . S.prepareTokens

-- | Call function with token trees.
--
--   >>> withTrees Right "a"
--   Right [TreeL (TText /0.1.0/ TextRaw "a")]
--
withTrees :: (ToTrees a) => ([Tree] -> B.Ab b) -> a -> B.Ab b
withTrees f a = f O.# toTrees a

-- | Read clauses and convert to token trees.
readClauseTrees :: FilePath -> B.IOAb [[Tree]]
readClauseTrees path =
    do ls' <- S.readClauses path
       return $ case ls' of
         Right ls   -> toTrees O.<#> ls
         Left a     -> Left a

