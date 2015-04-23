{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tree of tokens

module Koshucode.Baala.Base.Token.TokenTree
  ( -- * Token tree
    TTree,
    NamedTree, NamedTrees,
    TTreeTo, TTreesTo,
    TTreeToAb, TTreesToAb,
    ttrees,
    ttreeGroup,

    -- * Pattern
    pattern TermLeaf,
    pattern TermLeafPath,
    pattern TermLeafLocal,
    pattern TextLeaf,
    pattern TextLeafRaw,
    pattern TextLeafQ,
    pattern TextLeafQQ,
    pattern TextLeafKey,

    -- * Bracket type
    BracketType (..),
  
    -- * Divide trees
    splitTokensBy, divideTreesBy,
    divideTreesByBar, divideTreesByColon, divideTreesByEqual,
    divideTreesContents,
  
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
type TTree = B.CodeTree BracketType B.Token

-- | Pair of token trees and its name.
type NamedTrees = B.Named [TTree]

-- | Pair of token tree and its name.
type NamedTree = B.Named TTree

-- | Convert token tree to something.
type TTreeTo a    = TTree -> a

-- | Convert list of token tree to sometning.
type TTreesTo a   = [TTree] -> a

-- | Convert token tree to something, abortable.
type TTreeToAb a  = TTree -> B.Ab a

-- | Convert list of token tree to sometning, abortable.
type TTreesToAb a = [TTree] -> B.Ab a

-- local leaf
pattern TermLeafLocal cp v e ps = B.TreeL (B.TLocal cp v e ps)

-- term leaf
pattern TermLeaf      cp q ws   = B.TreeL (B.TTerm   cp q ws)
pattern TermLeafPath  cp ws     = TermLeaf cp B.TermTypePath ws

-- text leaf
pattern TextLeaf form cp w     = B.TreeL (B.TText   cp form w)
pattern TextLeafRaw   cp w     = TextLeaf B.TextRaw cp w
pattern TextLeafQ     cp w     = TextLeaf B.TextQ   cp w
pattern TextLeafQQ    cp w     = TextLeaf B.TextQQ  cp w
pattern TextLeafKey   cp w     = TextLeaf B.TextKey cp w

-- | Parse tokens with brackets into trees.
--   Blank tokens and comments are excluded.
ttrees :: [B.Token] -> B.Ab [TTree]
ttrees = B.trees getBracketType B.BracketNone where
    --und = map (B.undouble (== BracketGroup))

-- | Wrap trees in group.
ttreeGroup :: TTreesTo TTree
ttreeGroup = B.treeWrap BracketGroup


-- ----------------------  Bracket type

-- | There are six types of brackets
data BracketType
    = BracketGroup    -- ^ Round brackets for grouping: @( E ... )@
    | BracketForm     -- ^ Round-bar brackets for form with blanks: @(| V ... | E ... |)@
    | BracketList     -- ^ Square brackets for lists: @[ C : ... ]@
    | BracketSet      -- ^ Curely braces for sets: @{ C : .... }@
    | BracketRel      -- ^ Curely-bar braces for relations: @{| /N : ... | C : ... | C : ... |}@
    | BracketAssn     -- ^ Double-angle brackets for associations etc.: @\<\< /N C .... \>\>@
    | BracketInterp   -- ^ Triple-angle brackets for data interpretation: @\<\<\< ... /N ... \>\>\>@
    | BracketType     -- ^ Square-hyphen brackets for data type: @[- ... -]@
    | BracketUnknown  -- ^ Unknown bracket
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

getBracketType :: B.GetBracketType BracketType B.Token
getBracketType = B.bracketTable
    [ o BracketGroup   "("     ")"
    , o BracketForm    "(|"    "|)"
    , o BracketList    "["     "]"
    , o BracketSet     "{"     "}"
    , o BracketAssn    "<<"    ">>"
    , o BracketRel     "{|"    "|}"
    , o BracketInterp  "<<<"   ">>>"
    , o BracketType    "[-"    "-]"
    , (BracketUnknown, B.isOpenToken, B.isCloseToken)
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
    p2 (B.TTextRaw _ x)  = p x
    p2 _ = False

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: String -> TTreesTo [[TTree]]
divideTreesBy w1 = B.divideBy p where
    p (TextLeafRaw _ w2)  = w1 == w2
    p _                   = False

divideTreesBy2 :: String -> String -> TTreesTo [[TTree]]
divideTreesBy2 w1 w2 = B.divideBy p where
    p (TextLeafRaw _ w)  = w == w1 || w == w2
    p _                  = False

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: TTreesTo [[TTree]]
divideTreesByBar = divideTreesBy "|"

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: TTreesTo [[TTree]]
divideTreesByColon = divideTreesBy ":"

-- | Divide token trees by equal sign @\"=\"@.
divideTreesByEqual :: TTreesTo [[TTree]]
divideTreesByEqual = divideTreesBy "="

divideTreesContents :: TTreesTo [[TTree]]
divideTreesContents = divideTreesBy2 "|" ":"


-- ----------------------  Abortable

-- | Same as 'abortable' except for using 'B.TTree'
--   instead of list of 'B.Token'.
abortableTree :: String -> TTreeTo (B.Map (B.Ab b))
abortableTree tag = B.abortable tag . B.untree

-- | Same as 'abortable' except for using list of 'B.TTree'
--   instead of list of 'B.Token'.
abortableTrees :: String -> TTreesTo (B.Map (B.Ab b))
abortableTrees tag = B.abortable tag . B.untrees


-- ----------------------  Abbreviation

-- | Convert text to token trees.
tt :: String -> B.Ab [TTree]
tt s = do toks <- B.tokens (B.codeTextOf s) s
          ttrees $ B.sweepToken toks

tt1 :: String -> B.Ab TTree
tt1 = Right . ttreeGroup B.<=< tt

-- | Get 'B.Doc' value of token trees for pretty printing.
ttDoc :: TTreesTo B.Doc
ttDoc = dv where
    dv = B.docv . map d
    d (B.TreeL x) = B.doc "TreeL :" B.<+> B.doc x
    d (B.TreeB n pp xs) =
        let treeB = B.doch ["TreeB", show n] B.<+> brackets pp
        in P.hang treeB 2 (dv xs)

    brackets Nothing = B.doc "no brackets"
    brackets (Just (open, close)) = B.doch [B.doc ":", B.doc open, B.doc close]

ttPrint :: String -> IO ()
ttPrint s = case tt s of
              Left msg    -> print msg
              Right trees -> do print $ ttDoc trees
                                return ()
