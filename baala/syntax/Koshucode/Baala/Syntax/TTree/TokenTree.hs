{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tree of tokens

module Koshucode.Baala.Syntax.TTree.TokenTree
  ( -- * Type
    -- ** Tree
    TTree, NamedTree, NamedTrees,
    -- ** Conversion
    TTreeTo, TTreesTo,
    TTreeToAb, TTreesToAb,

    -- * Pattern
    -- ** Term leaf
    pattern TermLeaf,
    pattern TermLeafName,
    pattern TermLeafPath,
    pattern TermLeafLocal,
    -- ** Text leaf
    pattern TextLeaf,
    pattern TextLeafRaw,
    pattern TextLeafAttr,
    pattern TextLeafAttr2,
    pattern TextLeafQ,
    pattern TextLeafQQ,
    pattern TextLeafKey,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.TTree.Bracket    as S


-- --------------------------------------------  Token tree

-- | Tree of tokens.
type TTree = B.CodeTree S.BracketType S.Token
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


-- --------------------------------------------  Pattern

-- term leaf
pattern TermLeaf      cp q ws    = B.TreeL (S.TTerm   cp q ws)
pattern TermLeafName  cp sign w  = B.TreeL (S.TTermN  cp sign w)
pattern TermLeafPath  cp ws      = TermLeaf cp S.TermTypePath ws
pattern TermLeafLocal cp v e ps  = B.TreeL (S.TLocal cp v e ps)

-- | Text leaf.
pattern TextLeaf form cp w  = B.TreeL (S.TText   cp form w)
-- | Text leaf of 'S.TextRaw'.
pattern TextLeafRaw   cp w  = TextLeaf S.TextRaw cp w
-- | Text leaf beginning with single hyphen.
pattern TextLeafAttr  cp w  = TextLeaf S.TextRaw cp ('-' : w)
-- | Text leaf beginning with double hyphens.
pattern TextLeafAttr2 cp w  = TextLeaf S.TextRaw cp ('-' : '-' : w)
-- | Text leaf of 'S.TextQ'.
pattern TextLeafQ     cp w  = TextLeaf S.TextQ   cp w
-- | Text leaf of 'S.TextQQ'.
pattern TextLeafQQ    cp w  = TextLeaf S.TextQQ  cp w
-- | Text leaf of 'S.TextKey'.
pattern TextLeafKey   cp w  = TextLeaf S.TextKey cp w
