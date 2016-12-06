{-# OPTIONS_GHC -Wall #-}

-- | Tree of tokens

module Koshucode.Baala.Syntax.TTree.TokenTree
  ( TTree, NamedTree, NamedTrees,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.TTree.Bracket    as S

-- | Tree of tokens.
type TTree = B.CodeTree S.BracketType S.Token

-- | Pair of token trees and its name.
type NamedTrees = B.Named [TTree]

-- | Pair of token tree and its name.
type NamedTree = B.Named TTree

