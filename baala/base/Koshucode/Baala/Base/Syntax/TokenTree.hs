{-# OPTIONS_GHC -Wall #-}

{-| Parened tree of tokens -}

module Koshucode.Baala.Base.Syntax.TokenTree
(
  -- * Library
  TokenTree,
  tokenTrees,
  treeTokens,
  treesTokens,
  singleTree,
  flatname,

  -- * Divide trees
  splitTokens,
  divideTreesBy,
  divideTreesByBar,
  divideTreesByColon,

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
tokenTrees = map (undouble (== 0)) . trees parenType . sweepToken

treesTokens :: [TokenTree] -> [Token]
treesTokens = untrees typeParen

treeTokens :: TokenTree -> [Token]
treeTokens = untree typeParen

typeParen :: ParenType -> (Token, Token)
typeParen = o where
    o 1   =  p  "("  ")"
    o 2   =  p  "["  "]"
    o 3   =  p  "{"  "}"
    o 4   =  p  "<|" "|>"
    o 5   =  p  "{|" "|}"
    o _   =  p  "?"  "?"
    p a b =  ( TOpen 0 a, TClose 0 b )

parenType :: GetParenType Token
parenType = parenTable
    [ o 1  "("   ")"   -- grouping
    , o 2  "["   "]"   -- list
    , o 3  "{"   "}"   -- set
    , o 4  "<|" "|>"   -- termset
    , o 5  "{|" "|}"   -- relation
    ] where o n a b = (n, isOpenTokenOf a, isCloseTokenOf b)

singleTree :: [TokenTree] -> TokenTree
singleTree [t] = t
singleTree ts  = TreeB 1 ts

flatname :: TokenTree -> Maybe String
flatname (TreeL (TTerm _ [n])) = Just n
flatname _ = Nothing



-- ----------------------  Divide trees

{-| Split token list by unquoted word.
    If token list contains the word,
    pair of /before-list/, /the-word/ and /after-list/ is returned.
    If does not contains the word,
    original token list is returned.

    >>> splitTokens "|" . tokens $ "b c"
    Left [ TWord 1 0 "b", TSpace 2 1, TWord 3 0 "c" ]

    >>> splitTokens "|" . tokens $ "a | b | c"
    Right ( [ TWord 1 0 "a", TSpace 2 1 ]
            , TWord 3 0 "|"
            , [ TSpace 4 1, TWord 5 0 "b", TSpace 6 1
              , TWord 7 0 "|", TSpace 8 1, TWord 9 0 "c" ] )  -}
splitTokens
    :: String   -- ^ Word
    -> [Token]  -- ^ Tokens
    -> Either [Token] ([Token], Token, [Token])
       -- ^ Original-tokens or @(@before-list, the-word, after-list@)@
splitTokens w = splitBy p where
    p (TWord _ 0 x) = (w == x)
    p _ = False

divideTreesBy :: String -> [TokenTree] -> [[TokenTree]]
divideTreesBy w = divideBy p where
    p (TreeL (TWord _ 0 x)) = (w == x)
    p _ = False

{-| Divide token trees by vertical bar @\"|\"@.

    >>> divideTreesByBar . tokenTrees . tokens $ "a | b | c"
    [ [TreeL (TWord 1 0 "a")]
    , [TreeL (TWord 5 0 "b")]
    , [TreeL (TWord 9 0 "c")] ]  -}
divideTreesByBar :: [TokenTree] -> [[TokenTree]]
divideTreesByBar = divideTreesBy "|"

{-| Divide token trees by colon @\":\"@.

    >>> divideTreesByColon . tokenTrees . tokens $ "a : b : c"
    [ [TreeL (TWord 1 0 "a")]
    , [TreeL (TWord 5 0 "b")]
    , [TreeL (TWord 9 0 "c")] ]  -}
divideTreesByColon :: [TokenTree] -> [[TokenTree]]
divideTreesByColon = divideTreesBy ":"



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

