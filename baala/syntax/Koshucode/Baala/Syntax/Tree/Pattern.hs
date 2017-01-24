{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Patterns for token tree.

module Koshucode.Baala.Syntax.Tree.Pattern
  ( -- * Leaf
    pattern L,
    pattern LText,
    pattern LRaw, pattern LAtt1, pattern LAtt2,
    pattern LQ, pattern LQq,
    pattern LSlot,
    pattern LTerm,

    -- * Branch
    pattern B,
    pattern BGroup,
    pattern BList,
    pattern BSet,
    pattern BTerm,
    pattern BForm,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Token            as S
import qualified Koshucode.Baala.Syntax.Tree.Bracket     as S
import qualified Koshucode.Baala.Syntax.Token.Pattern    as P

-- ---------------------------------  Leaf

-- | Token leaf.
pattern L tok <- B.TreeL tok

-- | Text leaf.
pattern LText f s <- L (P.T f s)

-- | Raw text leaf.
pattern LRaw s <- L (P.TRaw s)

-- | Single-hyphen attribute text leaf.
pattern LAtt1 s <- LRaw (O.tCut -> O.Jp '-' s)

-- | Double-hyphen attribute text leaf.
pattern LAtt2 s <- LRaw (O.tCut2 -> O.Jp2 '-' '-' s)

-- | Single-quoted text leaf.
pattern LQ s <- L (P.TQ s)

-- | Double-quoted text leaf.
pattern LQq s <- L (P.TQq s)

-- | Slot leaf.
pattern LSlot n s <- L (S.TSlot _ n s)

-- | Term leaf.
pattern LTerm s <- L (P.Term s)

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

