{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tree of tokens

module Koshucode.Baala.Data.Token.TokenTree
  ( -- * Token tree
    -- ** Tree type
    TTree, NamedTree, NamedTrees,
    -- ** Conversion
    TTreeTo, TTreesTo,
    TTreeToAb, TTreesToAb,
    -- ** Parser
    ttrees, ttreeGroup,

    -- * Pattern
    -- ** Term
    pattern TermLeaf,
    pattern TermLeafName,
    pattern TermLeafPath,
    pattern TermLeafLocal,
    -- ** Text
    pattern TextLeaf,
    pattern TextLeafRaw,
    pattern TextLeafAttr,
    pattern TextLeafAttr2,
    pattern TextLeafQ,
    pattern TextLeafQQ,
    pattern TextLeafKey,

    -- * Divide trees
    splitTokensBy, divideTreesBy,
    divideTreesByBar, divideTreesByColon, divideTreesByEqual,
  
    -- * Abbreviation
    tt, tt1, ttPrint, ttDoc,
  ) where

import qualified Text.PrettyPrint                      as P
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Data.Token.Token      as D
import qualified Koshucode.Baala.Data.Token.TokenLine  as D
import qualified Koshucode.Baala.Data.Token.Bracket    as D


-- ---------------------- Token tree

-- | Tree of tokens.
type TTree = B.CodeTree D.BracketType D.Token

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
pattern TermLeafLocal cp v e ps = B.TreeL (D.TLocal cp v e ps)

-- term leaf
pattern TermLeaf      cp q ws    = B.TreeL (D.TTerm   cp q ws)
pattern TermLeafName  cp sign w  = B.TreeL (D.TTermN  cp sign w)
pattern TermLeafPath  cp ws      = TermLeaf cp D.TermTypePath ws

-- | Text leaf.
pattern TextLeaf form cp w  = B.TreeL (D.TText   cp form w)
-- | Text leaf of 'D.TextRaw'.
pattern TextLeafRaw   cp w  = TextLeaf D.TextRaw cp w
-- | Text leaf beginning with single hyphen.
pattern TextLeafAttr  cp w  = TextLeaf D.TextRaw cp ('-' : w)
-- | Text leaf beginning with double hyphens.
pattern TextLeafAttr2 cp w  = TextLeaf D.TextRaw cp ('-' : '-' : w)
-- | Text leaf of 'D.TextQ'.
pattern TextLeafQ     cp w  = TextLeaf D.TextQ   cp w
-- | Text leaf of 'D.TextQQ'.
pattern TextLeafQQ    cp w  = TextLeaf D.TextQQ  cp w
-- | Text leaf of 'D.TextKey'.
pattern TextLeafKey   cp w  = TextLeaf D.TextKey cp w

-- | Parse tokens with brackets into trees.
--   Blank tokens and comments are excluded.
ttrees :: [D.Token] -> B.Ab [TTree]
ttrees = B.trees D.getBracketType B.BracketNone where
    --und = map (B.undouble (== BracketGroup))

-- | Wrap trees in group.
ttreeGroup :: TTreesTo TTree
ttreeGroup = B.treeWrap D.BracketGroup


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
    -> [D.Token]       -- ^ Tokens
    -> Either [D.Token] ([D.Token], D.Token, [D.Token])
       -- ^ Original-tokens or @(@before-list, the-word, after-list@)@
splitTokensBy p = B.splitBy p2 where
    p2 (D.TTextRaw _ x)  = p x
    p2 _ = False

-- | Divide token trees by quoteless token of given string.
divideTreesBy :: String -> TTreesTo [[TTree]]
divideTreesBy w1 = B.divideBy p where
    p (TextLeafRaw _ w2)  = w1 == w2
    p _                   = False

-- | Divide token trees by vertical bar @\"|\"@.
divideTreesByBar :: TTreesTo [[TTree]]
divideTreesByBar = divideTreesBy "|"

-- | Divide token trees by colon @\":\"@.
divideTreesByColon :: TTreesTo [[TTree]]
divideTreesByColon = divideTreesBy ":"

-- | Divide token trees by equal sign @\"=\"@.
divideTreesByEqual :: TTreesTo [[TTree]]
divideTreesByEqual = divideTreesBy "="


-- ----------------------  Abbreviation

-- | Convert text to token trees.
tt :: String -> B.Ab [TTree]
tt s = do ts <- D.toks s
          ttrees $ D.sweepToken ts

-- | Parse string and group it.
tt1 :: String -> B.Ab TTree
tt1 = Right . ttreeGroup B.<=< tt

-- | Parse string and print it.
ttPrint :: String -> IO ()
ttPrint s = case tt s of
              Left msg    -> print msg
              Right trees -> do print $ ttDoc trees
                                return ()

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

