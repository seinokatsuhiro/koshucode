{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Patterns for token tree.

module Koshucode.Baala.Syntax.TTree.Pattern
  ( pattern L,
    pattern B,
    pattern LText,
    pattern LRaw,
    pattern LQq,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S

-- | Token leaf.
pattern L tok <- B.TreeL tok

-- | Token branch.
pattern B b xs <- B.TreeB b _ xs

-- | Text leaf.
pattern LText f s <- L (S.TText _ f s)

-- | Raw text leaf.
pattern LRaw s <- LText S.TextRaw s

-- | Double-quoted text leaf.
pattern LQq s <- LText S.TextQQ s

