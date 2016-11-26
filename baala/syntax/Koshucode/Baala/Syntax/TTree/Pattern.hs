{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Patterns for token tree.

module Koshucode.Baala.Syntax.TTree.Pattern
  ( -- * Leaf
    pattern L,
    pattern LText,
    pattern LRaw,
    pattern LQq,
    -- * Branch
    pattern B,
    pattern BGroup,
    pattern BList,
    pattern BSet,
    pattern BTerm,
    pattern BForm,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.TTree.Bracket    as S

-- ---------------------------------  Leaf

-- | Token leaf.
pattern L tok <- B.TreeL tok

-- | Text leaf.
pattern LText f s <- L (S.TText _ f s)

-- | Raw text leaf.
pattern LRaw s <- LText S.TextRaw s

-- | Double-quoted text leaf.
pattern LQq s <- LText S.TextQQ s

-- ---------------------------------  Branch

-- | Token branch.
pattern B b xs <- B.TreeB b _ xs

-- | Group branch.
pattern BGroup xs <- B S.BracketGroup xs

-- | List branch.
pattern BList xs <- B S.BracketList xs

-- | Set branch.
pattern BSet xs <- B S.BracketSet xs

-- | Term path branch.
pattern BTerm xs <- B S.BracketTerm xs

-- | Form branch.
pattern BForm xs <- B S.BracketForm xs

