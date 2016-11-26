{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tree of tokens

module Koshucode.Baala.Syntax.TTree.TokenTree
  ( -- * Type
    TTree, NamedTree, NamedTrees,

    -- * Pattern
    pattern TermLeafName,
    pattern TextLeaf,
    pattern TextLeafRaw,
    pattern TextLeafQ,
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


-- --------------------------------------------  Pattern

-- | Term leaf.
{-# DEPRECATED TermLeafName "Use LTerm instead." #-}
pattern TermLeafName  cp w  = B.TreeL (S.TTerm  cp w)

-- | Text leaf.
pattern TextLeaf form cp w  = B.TreeL (S.TText   cp form w)

-- | Text leaf of 'S.TextRaw'.
{-# DEPRECATED TextLeafRaw "Use LRaw instead." #-}
pattern TextLeafRaw   cp w  = TextLeaf S.TextRaw cp w

-- | Text leaf of 'S.TextQ'.
pattern TextLeafQ     cp w  = TextLeaf S.TextQ   cp w
