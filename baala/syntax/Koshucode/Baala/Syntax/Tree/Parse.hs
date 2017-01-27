{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parser for token tree.

module Koshucode.Baala.Syntax.Tree.Parse
  ( -- * Type
    Tree, TTree,

    -- * Parser
    ToTrees (..),
    ttreeGroup,
    withTrees,
    readClauseTrees,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Symbol           as S
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.Tree.Bracket     as S


-- --------------------------------------------  Type

-- | Tree of tokens.
type Tree = TTree String

-- | Tree of tokens.
type TTree t = B.CodeTree S.BracketType S.TToken t

-- | Wrap trees in group.
ttreeGroup :: [TTree t] -> TTree t
ttreeGroup = B.codeTreeWrap S.BracketGroup


-- --------------------------------------------  Parser

-- | Convert to token trees.
class ToTrees t a where
    -- | Convert to list of token trees.
    toTrees :: (S.TextualTermName t) => a -> B.Ab [TTree t]

    -- | Convert to token tree.
    toTree :: (S.TextualTermName t) => a -> B.Ab (TTree t)
    toTree a = ttreeGroup <$> toTrees a

    -- | List of token trees or error.
    toTrees' :: (S.TextualTermName t) => a -> [TTree t]
    toTrees' = B.unabort . toTrees

    -- | Token tree or error.
    toTree' :: (S.TextualTermName t) => a -> TTree t
    toTree' = B.unabort . toTree

instance ToTrees t String where
    toTrees = tokenTrees . S.toks

instance ToTrees t O.Tx where
    toTrees = toTrees . B.txString

instance ToTrees t O.Tz where
    toTrees = toTrees . B.tzString

instance ToTrees t O.Bz where
    toTrees = toTrees . B.bzString

instance ToTrees t O.Bs where
    toTrees = toTrees . B.bsString

instance ToTrees t [S.TToken t] where
    toTrees = tokenTrees

instance ToTrees t [TTree t] where
    toTrees = Right

instance ToTrees t (S.TokenClause t) where
    toTrees = toTrees . B.clauseTokens

-- | Parse tokens with brackets into trees.
--   Blank tokens and comments are excluded.
tokenTrees :: (O.Textual t, B.IsString t) => [S.TToken t] -> B.Ab [TTree t]
tokenTrees = B.codeTrees S.getBracketType B.BracketNone . S.prepareTokens

-- | Call function with token trees.
--
--   >>> withTrees Right "a"
--   Right [TreeL (TText /0.1.0/ TextRaw "a")]
--
withTrees :: (S.TextualTermName t, ToTrees t a) => ([TTree t] -> B.Ab b) -> a -> B.Ab b
withTrees f a = f O.# toTrees a

-- | Read clauses and convert to token trees.
readClauseTrees :: FilePath -> B.IOAb [[TTree String]]
readClauseTrees path =
    do ls' <- S.readClauses path
       return $ case ls' :: B.Ab [S.TokenClause String] of
         Right ls   -> toTrees O.<#> ls
         Left a     -> Left a

