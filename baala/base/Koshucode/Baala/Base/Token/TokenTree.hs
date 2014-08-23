{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parened tree of tokens

module Koshucode.Baala.Base.Token.TokenTree
( -- * Library
  TokenTree,
  NamedTrees,
  tokenTrees,
  wrapTrees,

  -- * Paren type
  ParenType (..),

  -- * Abbreviation
  tt, tt1, ttDoc,
    
  -- * Divide trees
  splitTokensBy, divideTreesBy,
  divideTreesByBar, divideTreesByColon, divideTreesByEqual,

  -- * Abortable
  abortableTree, abortableTrees,

  -- * Examples
  -- $Example
) where

import qualified Data.Generics                        as G
import qualified Text.PrettyPrint                     as P
import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax          as B
import qualified Koshucode.Baala.Base.Text            as B
import qualified Koshucode.Baala.Base.Token.Token     as B
import qualified Koshucode.Baala.Base.Token.TokenLine as B



-- ----------------------

-- | Tree of tokens.
type TokenTree = B.CodeTree ParenType B.Token

-- | Pair of token trees and its name.
type NamedTrees = B.Named [TokenTree]

-- | Parse tokens into parened trees.
--   Blank tokens and comments are excluded.
--
--   There are four types of parens -- 1, 2, 3, or 4.
--   Paren type is in 'TreeB' /type/ /parens/ /subtrees/.
--
--   1. Round parens @( .. )@ for grouping.
--
--   2. Squared brackets @[ .. ]@ for list.
--
--   3. Double brackets @\<\< .. \>\>@ for assn.
--
--   4. Curely-bar braces @{| .. |}@ for relation.

tokenTrees :: [B.Token] -> B.Ab [TokenTree]
tokenTrees = B.trees parenType B.ParenNon where
    --und = map (B.undouble (== ParenGroup))

wrapTrees :: [TokenTree] -> TokenTree
wrapTrees = B.treeWrap ParenGroup

parenType :: B.GetParenType ParenType B.Token
parenType = B.parenTable
    [ o ParenGroup  "("    ")"   -- grouping
    , o ParenForm   "(|"  "|)"   -- function
    , o ParenList   "["    "]"   -- list
    , o ParenSet    "{"    "}"   -- set
    , o ParenAssn   "<<"  ">>"   -- assn (association)
    , o ParenRel    "{|"  "|}"   -- relation
    ] where o n a b = (n, B.isOpenTokenOf a, B.isCloseTokenOf b)

data ParenType
    = ParenGroup
    | ParenForm
    | ParenList
    | ParenSet
    | ParenAssn
    | ParenRel
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- ----------------------  Abbreviation

-- | Convert text to token trees.
tt :: String -> B.Ab [TokenTree]
tt s = tokenTrees $ B.sweepToken $ B.tokens (B.ResourceText s) s

tt1 :: String -> B.Ab TokenTree
tt1 = Right . wrapTrees B.<=< tt

-- | Get 'B.Doc' value of token trees for pretty printing.
ttDoc :: [TokenTree] -> B.Doc
ttDoc = dv where
    dv = B.docv . map d
    d (B.TreeL x) = B.doc "TreeL :" B.<+> B.doc x
    d (B.TreeB n pp xs) =
        let treeB = B.doch ["TreeB", show n] B.<+> parens pp
        in P.hang treeB 2 (dv xs)
    parens Nothing = B.doc "- -"
    parens (Just (open, close)) = B.doch [open, close]


-- ----------------------  Divide trees

-- | Split token list by unquoted word.
--   If token list contains the word,
--   pair of /before-list/, /the-word/ and /after-list/ is returned.
--   If does not contains the word,
--   original token list is returned.
--
--   >>> splitTokensBy (== "|") . tokens $ "b c"
--   Left [ TText 1 0 "b", TSpace 2 1, TText 3 0 "c" ]
--
--   >>> splitTokensBy (== "|") . tokens $ "a | b | c"
--   Right ( [ TText 1 0 "a", TSpace 2 1 ]
--           , TText 3 0 "|"
--           , [ TSpace 4 1, TText 5 0 "b", TSpace 6 1
--             , TText 7 0 "|", TSpace 8 1, TText 9 0 "c" ] )
--
splitTokensBy
    :: B.Pred String   -- ^ Predicate
    -> [B.Token]       -- ^ Tokens
    -> Either [B.Token] ([B.Token], B.Token, [B.Token])
       -- ^ Original-tokens or @(@before-list, the-word, after-list@)@
splitTokensBy p = B.splitBy p2 where
    p2 (B.TText _ 0 x) = p x
    p2 _ = False

divideTreesBy :: String -> [TokenTree] -> [[TokenTree]]
divideTreesBy w = B.divideBy p where
    p (B.TreeL (B.TText _ 0 x)) = (w == x)
    p _ = False

-- | Divide token trees by vertical bar @\"|\"@.
--
--   >>> divideTreesByBar . tokenTrees . tokens $ "a | b | c"
--   [ [TreeL (TText 1 0 "a")]
--   , [TreeL (TText 5 0 "b")]
--   , [TreeL (TText 9 0 "c")] ]
--
divideTreesByBar :: [TokenTree] -> [[TokenTree]]
divideTreesByBar = divideTreesBy "|"

-- | Divide token trees by colon @\":\"@.
--
--   >>> divideTreesByColon . tokenTrees . tokens $ "a : b : c"
--   [ [TreeL (TText 1 0 "a")]
--   , [TreeL (TText 5 0 "b")]
--   , [TreeL (TText 9 0 "c")] ]
--
divideTreesByColon :: [TokenTree] -> [[TokenTree]]
divideTreesByColon = divideTreesBy ":"

divideTreesByEqual :: [TokenTree] -> [[TokenTree]]
divideTreesByEqual = divideTreesBy "="


-- ----------------------  Abortable

-- | Same as 'abortable' except for using 'B.TokenTree'
--   instead of list of 'B.Token'.
abortableTree :: String -> TokenTree -> B.Map (B.Ab b)
abortableTree tag = B.abortable tag . B.untree

-- | Same as 'abortable' except for using list of 'B.TokenTree'
--   instead of list of 'B.Token'.
abortableTrees :: String -> [TokenTree] -> B.Map (B.Ab b)
abortableTrees tag = B.abortable tag . B.untrees


-- ------------------------------------------------------------------
-- $Example
--
--  Judgement
--
--  >>> tt "|-- R /x 0 /y 1"
--  [TreeL (TText 1 0 "|--"),
--   TreeL (TText 3 0 "R"),
--   TreeL (TTerm 5 ["/x"]),
--   TreeL (TText 7 0 "0"),
--   TreeL (TTerm 9 ["/y"]),
--   TreeL (TText 11 0 "1")]
--
--  Relmap
--
--  >>> tt "r : source R /x /y"
--  [TreeL (TText 1 0 "r"),
--   TreeL (TText 3 0 ":"),
--   TreeL (TText 5 0 "source"),
--   TreeL (TText 7 0 "R"),
--   TreeL (TTerm 9 ["/x"]),
--   TreeL (TTerm 11 ["/y"])]
--
--  Nested relmap
--
--  >>> tt "meet (R | pick /a /b)"
--  [TreeL (TText 1 0 "meet"),
--   TreeB 1 [
--     TreeL (TText 4 0 "R"),
--     TreeL (TText 6 0 "|"),
--     TreeL (TText 8 0 "pick"),
--     TreeL (TTerm 10 ["/a"]),
--     TreeL (TTerm 12 ["/b"])]]
--
--  Double paren
--
--  >>> tt "(a ((b c)))"
--  [TreeB 1 [
--     TreeL (TText 2 0 "a"),
--     TreeB 1 [
--       TreeB 1 [
--         TreeL (TText 6 0 "b"),
--         TreeL (TText 8 0 "c")]]]]
--
--  List
--
--  >>> tt "[ 0 1 2 ]"
--  [TreeB 2 [
--    TreeL (TText 3 0 "0"),
--    TreeL (TText 5 0 "1"),
--    TreeL (TText 7 0 "2")]]
--
--  Relation
--
--  >>> tt "{| /a /b | 10 20 | 30 40 |}"
--  [TreeB 4 [
--     TreeL (TTerm 3 ["/a"]), TreeL (TTerm 5 ["/b"]),
--     TreeL (TText 7 0 "|"),
--     TreeL (TText 9 0 "10"), TreeL (TText 11 0 "20"),
--     TreeL (TText 13 0 "|"),
--     TreeL (TText 15 0 "30"), TreeL (TText 17 0 "40")]]
--

