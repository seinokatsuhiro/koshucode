{-# OPTIONS_GHC -Wall #-}

-- | Parened tree of tokens

module Koshucode.Baala.Base.Syntax.TokenTree
( TokenTree
, tokenTrees
, tokenTreesSource
) where

import Koshucode.Baala.Base.Syntax.Token
import Koshucode.Baala.Base.Syntax.Tokenizer
import Koshucode.Baala.Base.Syntax.Tree

{-| Tree of tokens. -}
type TokenTree = Tree Token

{-| Parse tokens into parened trees.
    Blank tokens are excluded.

    >>> tokenTrees $ tokens $ "meet (R | pick /a /b)"
    [TreeL (Word 0 "meet"),
     TreeB 1 [
       TreeL (Word 0 "R"),
       TreeL (Word 0 "|"),
       TreeL (Word 0 "pick"),
       TreeL (TermN ["/a"]),
       TreeL (TermN ["/b"])]]

    There are three types of parens -- 1, 2, or 3.
    Paren type is in 'TreeB' /type/ /subtrees/.

    1. Round parens @()@

    2. Squared brackets @[]@

    3. Curely braces @{}@
  -}
tokenTrees :: [Token] -> [TokenTree]
tokenTrees = trees parenType . sweepToken

parenType :: ParenType Token
typeParen :: TypeParen Token
(parenType, typeParen) = parenTable
      [ (1, TOpen 0 "(", TClose 0 ")")
      , (2, TOpen 0 "[", TClose 0 "]")
      , (3, TOpen 0 "{", TClose 0 "}")
      ]

{-| Convert back tree into source string. -}
tokenTreesSource :: [TokenTree] -> String
tokenTreesSource = untokens . tokenUntrees

{-| Convert back tree into list of tokens. -}
tokenUntrees :: [TokenTree] -> [Token]
tokenUntrees = untrees typeParen

-- e1 = tokenTreesSource . tokenTrees . tokens
-- e2 = e1 ""
-- e3 = e1 "/a"
-- e4 = e1 "/a (/x + 1)"
-- e5 = e1 "(a b [[c d]] {e f})"

