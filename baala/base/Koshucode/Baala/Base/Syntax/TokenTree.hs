{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.TokenTree
( TokenTree
, tokenTree, tokenTrees
, tokenTreesSource
, module Koshucode.Baala.Base.Syntax.Token
, module Koshucode.Baala.Base.Syntax.Tree
) where

import Koshucode.Baala.Base.Syntax.Token
import Koshucode.Baala.Base.Syntax.Tree

-- | Tree of tokens
type TokenTree = Tree Token

-- | Parse tokens into a parened tree.
tokenTree :: [Token] -> TokenTree
tokenTree = Branch 0 . tokenTrees

-- | Parse tokens into parened trees.
tokenTrees :: [Token] -> [TokenTree]
tokenTrees = trees parenType . sweep

parenType :: ParenType Token
typeParen :: TypeParen Token
(parenType, typeParen) = parenTable
      [ (1, Open "(", Close ")"),
        (2, Open "[", Close "]"),
        (3, Open "{", Close "}") ]

-- | Convert tree to list of tokens
tokenUntrees :: [TokenTree] -> [Token]
tokenUntrees = untrees typeParen

tokenTreesSource :: [TokenTree] -> String
tokenTreesSource = untokens . tokenUntrees

-- e1 = tokenTreesSource . tokenTrees . tokens
-- e2 = e1 ""
-- e3 = e1 "/a"
-- e4 = e1 "/a (/x + 1)"
-- e5 = e1 "(a b [[c d]] {e f})"

