{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parened tree of tokens

module Koshucode.Baala.Base.Token.TokenTree
( -- * Token tree
  TokenTree,
  NamedTrees,
  tokenTrees,
  wrapTrees,

  -- * Paren type
  ParenType (..),

  -- * Divide trees
  splitTokensBy, divideTreesBy,
  divideTreesByBar, divideTreesByColon, divideTreesByEqual,

  -- * Abortable
  abortableTree, abortableTrees,

  -- * Abbreviation
  tt, tt1, ttDoc, ttPrint,
) where

import qualified Data.Generics                        as G
import qualified Text.PrettyPrint                     as P
import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax          as B
import qualified Koshucode.Baala.Base.Text            as B
import qualified Koshucode.Baala.Base.Token.Token     as B
import qualified Koshucode.Baala.Base.Token.TokenLine as B


-- ---------------------- Token tree

-- | Tree of tokens.
type TokenTree = B.CodeTree ParenType B.Token

-- | Pair of token trees and its name.
type NamedTrees = B.Named [TokenTree]

-- | Parse tokens into parened trees.
--   Blank tokens and comments are excluded.
tokenTrees :: [B.Token] -> B.Ab [TokenTree]
tokenTrees = B.trees getParenType B.ParenNon where
    --und = map (B.undouble (== ParenGroup))

-- | Wrap trees in group.
wrapTrees :: [TokenTree] -> TokenTree
wrapTrees = B.treeWrap ParenGroup


-- ----------------------  Paren type

-- | There are six types of parens
data ParenType
    = ParenGroup   -- ^ Round parens for grouping: @( E ... )@
    | ParenForm    -- ^ Round-bar parens for form with blanks: @(| V ... | E ... |)@
    | ParenList    -- ^ Square brackets for lists: @[ C : ... ]@
    | ParenSet     -- ^ Curely braces for sets: @{ C : .... }@
    | ParenAssn    -- ^ Double-angle brackets for associations etc.: @\<\< /N C .... \>\>@
    | ParenRel     -- ^ Curely-bar braces for relations: @{| /N : ... | C : ... | C : ... |}@
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

getParenType :: B.GetParenType ParenType B.Token
getParenType = B.parenTable
    [ o ParenGroup  "("     ")"
    , o ParenForm   "(|"   "|)"
    , o ParenList   "["     "]"
    , o ParenSet    "{"     "}"
    , o ParenAssn   "<<"   ">>"
    , o ParenRel    "{|"   "|}"
    ] where o n a b = (n, B.isOpenTokenOf a, B.isCloseTokenOf b)


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

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: String -> [TokenTree] -> [[TokenTree]]
divideTreesBy w1 = B.divideBy p where
    p (B.TreeL (B.TText _ 0 w2))  =  w1 == w2
    p _                           =  False

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: [TokenTree] -> [[TokenTree]]
divideTreesByBar = divideTreesBy "|"

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: [TokenTree] -> [[TokenTree]]
divideTreesByColon = divideTreesBy ":"

-- | Divide token trees by equal sign @\"=\"@.
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

    parens Nothing = B.doc "no parens"
    parens (Just (open, close)) = B.doch [B.doc ":", B.doc open, B.doc close]

ttPrint :: String -> IO ()
ttPrint s = case tt s of
              Left msg    -> print msg
              Right trees -> do print $ ttDoc trees
                                return ()
