{-# OPTIONS_GHC -Wall #-}

{-| Minimal operand patterns. -}

module Koshucode.Baala.Minimal.Operand
( -- * Operand pattern
  MinimalOperand (..),

  -- * Operand sorter
  likePick,
  likeMeet,
  likeSource,
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin



-- ----------------------  Operand paterns

{- | 'Builtin.RopPattern' for minimal operators -}
data MinimalOperand
    = LikeId
    | LikeMeet      -- ^ 'likeMeet' sorter
    | LikePick      -- ^ 'likePick' sorter
    | LikeSource    -- ^ 'likeSource' sorter
      deriving (Show, Eq, Enum)

instance Builtin.RopPattern MinimalOperand where
    ropSorter   LikeId     = id
    ropSorter   LikeMeet   = likeMeet
    ropSorter   LikePick   = likePick
    ropSorter   LikeSource = likeSource

    ropPart     LikeId     = []
    ropPart     LikeMeet   = ["-relmap", "-share"]
    ropPart     LikePick   = ["-term"]
    ropPart     LikeSource = ["-sign", "-term"]



-- ----------------------  Sorter

{-| This sorter recognizes @-term@ operand.

    > pick /a /b
    > pick -term /a /b  -}
likePick       :: C.RopSorter
likePick       =  C.ropPartName "-term"

{-| This sorter recognizes @-relmap@ operand.

    > meet a
    > meet -relmap a  -}
likeMeet       :: C.RopSorter
likeMeet       =  C.ropPartName "-relmap"

{-| This sorter recognizes @-sign@ and @-term@ operand.

    > source A /a /b
    > source -sign A -term /a /b  -}
likeSource     :: C.RopSorter
likeSource     =  C.ropPartNameBy f where
    f (s : ts) = [ ("-sign", [s]), ("-term", ts) ]
    f _        = []

