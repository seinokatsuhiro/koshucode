{-# OPTIONS_GHC -Wall #-}

-- | Operand patterns

module Koshucode.Baala.Minimal.Operand
( -- * Operand patterns
  MinimalOperand (..),

  -- * Operand parsers
  likePick,
  likeRename,
  likeMeet,
  likeSource,
) where

import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin hiding (LikeId)



-- ----------------------  Opernd paterns

-- | 'RopPattern' for minimal operators
data MinimalOperand
    = LikeId      -- ^ no operand
    | LikeMeet    -- ^ { @-relmap@ } relmap [ @-share@ \/name ... ]
    | LikePick    -- ^ { @-term@ } \/name ...
    | LikeRename  -- ^ { @-term@ } \/new \/old ...
    | LikeSource  -- ^ { @-sign@ } relsign { @-term@ } \/name ...
      deriving (Show, Eq, Enum)

instance RopPattern MinimalOperand where
    ropSorter   LikeId     = id
    ropSorter   LikeMeet   = likeMeet
    ropSorter   LikePick   = likePick
    ropSorter   LikeRename = likeRename
    ropSorter   LikeSource = likeSource

    ropPart     LikeId     = []
    ropPart     LikeMeet   = ["-relmap", "-share"]
    ropPart     LikePick   = ["-term"]
    ropPart     LikeRename = ["-term"]
    ropPart     LikeSource = ["-sign", "-term"]

    ropUsage    LikeId     = [""]
    ropUsage    LikeMeet   = ["RELMAP [-share /NAME ...]"]
    ropUsage    LikePick   = ["/NAME ..."]
    ropUsage    LikeRename = ["/NEW /OLD ..."]
    ropUsage    LikeSource = ["RELSIGN /NAME ..."]



-- ----------------------  Opernd parsers

likePick    :: C.RopSorter
likePick    =  C.ropPartName "-term"

likeRename  :: C.RopSorter
likeRename  =  C.ropPartName "-term"

likeMeet    :: C.RopSorter
likeMeet    =  C.ropPartName "-relmap"

likeSource  :: C.RopSorter
likeSource  =  C.ropPartNameBy f where
    f (s:ns) = [("-sign", [s]), ("-term", ns)]
    f _      = []

