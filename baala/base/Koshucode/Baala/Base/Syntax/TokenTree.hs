{-# OPTIONS_GHC -Wall #-}

-- | Parened tree of tokens

module Koshucode.Baala.Base.Syntax.TokenTree
(
-- * Library
  TokenTree
, tokenTrees

-- * Examples
-- $Example
) where

import Koshucode.Baala.Base.Syntax.Token
import Koshucode.Baala.Base.Syntax.Tree

{-| Tree of tokens. -}
type TokenTree = Tree Token

{-| Parse tokens into parened trees.
    Blank tokens are excluded.

    There are three types of parens -- 1, 2, or 3.
    Paren type is in 'TreeB' /type/ /subtrees/.

    1. Round parens @()@

    2. Squared brackets @[]@

    3. Curely braces @{}@
  -}
tokenTrees :: [Token] -> [TokenTree]
tokenTrees = trees parenType . sweepToken

parenType :: ParenType Token
parenType = parenTable
      [ (1, isOpenTokenOf "(", isCloseTokenOf ")")
      , (2, isOpenTokenOf "[", isCloseTokenOf "]")
      , (3, isOpenTokenOf "{", isCloseTokenOf "}")
      ]

-- ----------------------
{- $Example

   Before evaluating the following examples,
   please import @Tokenizer@ module to use the @tokens@ function.
   
   >>> :m +Koshucode.Baala.Base.Syntax.Tokenizer

   Tuple.

   >>> tokenTrees . tokens $ "|-- R /x 0 /y 1"
   [TreeL (TWord 1 0 "|--"),
    TreeL (TWord 3 0 "R"),
    TreeL (TTermN 5 ["/x"]),
    TreeL (TWord 7 0 "0"),
    TreeL (TTermN 9 ["/y"]),
    TreeL (TWord 11 0 "1")]

   List.

   >>> tokenTrees . tokens $ "[ 0 1 2 ]"
   [TreeB 2 [
     TreeL (TWord 3 0 "0"),
     TreeL (TWord 5 0 "1"),
     TreeL (TWord 7 0 "2")]]

   Relmap.

   >>> tokenTrees . tokens $ "r : source R /x /y"
   [TreeL (TWord 1 0 "r"),
    TreeL (TWord 3 0 ":"),
    TreeL (TWord 5 0 "source"),
    TreeL (TWord 7 0 "R"),
    TreeL (TTermN 9 ["/x"]),
    TreeL (TTermN 11 ["/y"])]

   Nested relmap.

   >>> tokenTrees . tokens $ "meet (R | pick /a /b)"
   [TreeL (TWord 1 0 "meet"),
    TreeB 1 [
      TreeL (TWord 4 0 "R"),
      TreeL (TWord 6 0 "|"),
      TreeL (TWord 8 0 "pick"),
      TreeL (TTermN 10 ["/a"]),
      TreeL (TTermN 12 ["/b"])]]

   Double paren.

   >>> tokenTrees . tokens $ "(a ((b c)))"
   [TreeB 1 [
      TreeL (TWord 2 0 "a"),
      TreeB 1 [
        TreeB 1 [
          TreeL (TWord 6 0 "b"),
          TreeL (TWord 8 0 "c")]]]]
-}

