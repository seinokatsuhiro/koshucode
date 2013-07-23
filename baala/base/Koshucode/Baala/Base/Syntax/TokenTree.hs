{-# OPTIONS_GHC -Wall #-}

-- | Parened tree of tokens

module Koshucode.Baala.Base.Syntax.TokenTree
(
-- * Library
  TokenTree
, tokenTrees
, singleTree
, divideByTokenTree

-- * Examples
-- $Example
) where

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Syntax.Token
import Koshucode.Baala.Base.Syntax.Tree

-- ----------------------

{-| Tree of tokens. -}
type TokenTree = Tree Token

{-| Parse tokens into parened trees.
    Blank tokens and comments are excluded.

    There are four types of parens -- 1, 2, 3, or 4.
    Paren type is in 'TreeB' /type/ /subtrees/.

    1. Round parens @( .. )@ for grouping.

    2. Squared brackets @[ .. ]@ for list.

    3. Curely braces @{ .. }@ for termset.

    4. Curely-bar braces @{| .. |}@ for relation.
  -}
tokenTrees :: [Token] -> [TokenTree]
tokenTrees = trees parenType . sweepToken

parenType :: ParenType Token
parenType = parenTable
    [ (1, isOpenTokenOf "("  , isCloseTokenOf ")")   -- grouping
    , (2, isOpenTokenOf "["  , isCloseTokenOf "]")   -- list
    , (3, isOpenTokenOf "{"  , isCloseTokenOf "}")   -- set
    , (4, isOpenTokenOf "<|" , isCloseTokenOf "|>")  -- termset
    , (5, isOpenTokenOf "{|" , isCloseTokenOf "|}")  -- relation
    , (5, isOpenTokenOf "[|" , isCloseTokenOf "|]")  -- relation
    ]

singleTree :: [TokenTree] -> TokenTree
singleTree [t] = t
singleTree ts  = TreeB 1 ts

divideByTokenTree :: String -> [TokenTree] -> [[TokenTree]]
divideByTokenTree w = divideByP p where
    p (TreeL (TWord _ 0 x)) | w == x = True
    p _ = False



-- ----------------------
{- $Example

   Before evaluating the following examples,
   please import @Tokenizer@ module to use the @tokens@ function.
   
   >>> :m +Koshucode.Baala.Base.Syntax.Tokenizer

   Tuple.

   >>> tokenTrees . tokens $ "|-- R /x 0 /y 1"
   [TreeL (TWord 1 0 "|--"),
    TreeL (TWord 3 0 "R"),
    TreeL (TTerm 5 ["/x"]),
    TreeL (TWord 7 0 "0"),
    TreeL (TTerm 9 ["/y"]),
    TreeL (TWord 11 0 "1")]

   Relmap.

   >>> tokenTrees . tokens $ "r : source R /x /y"
   [TreeL (TWord 1 0 "r"),
    TreeL (TWord 3 0 ":"),
    TreeL (TWord 5 0 "source"),
    TreeL (TWord 7 0 "R"),
    TreeL (TTerm 9 ["/x"]),
    TreeL (TTerm 11 ["/y"])]

   Nested relmap.

   >>> tokenTrees . tokens $ "meet (R | pick /a /b)"
   [TreeL (TWord 1 0 "meet"),
    TreeB 1 [
      TreeL (TWord 4 0 "R"),
      TreeL (TWord 6 0 "|"),
      TreeL (TWord 8 0 "pick"),
      TreeL (TTerm 10 ["/a"]),
      TreeL (TTerm 12 ["/b"])]]

   Double paren.

   >>> tokenTrees . tokens $ "(a ((b c)))"
   [TreeB 1 [
      TreeL (TWord 2 0 "a"),
      TreeB 1 [
        TreeB 1 [
          TreeL (TWord 6 0 "b"),
          TreeL (TWord 8 0 "c")]]]]

   List.

   >>> tokenTrees . tokens $ "[ 0 1 2 ]"
   [TreeB 2 [
     TreeL (TWord 3 0 "0"),
     TreeL (TWord 5 0 "1"),
     TreeL (TWord 7 0 "2")]]

   Relation.

   >>> tokenTrees . tokens $ "{| /a /b | 10 20 | 30 40 |}"
   [TreeB 4 [
      TreeL (TTerm 3 ["/a"]), TreeL (TTerm 5 ["/b"]),
      TreeL (TWord 7 0 "|"),
      TreeL (TWord 9 0 "10"), TreeL (TWord 11 0 "20"),
      TreeL (TWord 13 0 "|"),
      TreeL (TWord 15 0 "30"), TreeL (TWord 17 0 "40")]]
      

-}

