{-# OPTIONS_GHC -Wall #-}

-- | Parened tree of tokens

module Koshucode.Baala.Base.Syntax.TokenTree
( TokenTree
, tokenTrees
, tokenTreesSource
, module Koshucode.Baala.Base.Syntax.Token
, module Koshucode.Baala.Base.Syntax.Tree
) where

import Koshucode.Baala.Base.Syntax.Token
import Koshucode.Baala.Base.Syntax.Tree

-- | Tree of tokens
type TokenTree = Tree Token

-- | Parse tokens into parened trees.
--   Blank tokens are excluded.
-- 
--   >>> tokenTrees $ tokens $ "meet (R | pick /a /b)"
--   [TreeL (Word 0 "meet"),
--    TreeB 1 [
--      TreeL (Word 0 "R"),
--      TreeL (Word 0 "|"),
--      TreeL (Word 0 "pick"),
--      TreeL (TermN ["/a"]),
--      TreeL (TermN ["/b"])]]

tokenTrees :: [Token] -> [TokenTree]
tokenTrees = trees parenType . sweepToken

parenType :: ParenType Token
typeParen :: TypeParen Token
(parenType, typeParen) = parenTable
      [ (1, Open "(", Close ")"),
        (2, Open "[", Close "]"),
        (3, Open "{", Close "}") ]

-- | Convert back tree to list of tokens
tokenUntrees :: [TokenTree] -> [Token]
tokenUntrees = untrees typeParen

-- | Convert back tree to source string
tokenTreesSource :: [TokenTree] -> String
tokenTreesSource = untokens . tokenUntrees

-- e1 = tokenTreesSource . tokenTrees . tokens
-- e2 = e1 ""
-- e3 = e1 "/a"
-- e4 = e1 "/a (/x + 1)"
-- e5 = e1 "(a b [[c d]] {e f})"

