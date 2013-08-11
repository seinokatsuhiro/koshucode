{-# OPTIONS_GHC -Wall #-}

-- | Operand patterns

module Koshucode.Baala.Minimal.Operand
( -- * Operand patterns
  MinimalOperand (..),

  -- * Operand parsers
  likePick,
  likeMeet,
  likeRename,
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
    ropParser'  LikeId     = id
    ropParser'  LikeMeet   = likeMeet
    ropParser'  LikePick   = likePick
    ropParser'  LikeRename = likeRename
    ropParser'  LikeSource = likeSource

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

likePick :: C.RopParser'
likePick xs =
    case lookup "" xs of
      Just xs2 -> [("-term", xs2)] ++ xs
      _ -> xs

likeMeet :: C.RopParser'
likeMeet xs =
    case lookup "" xs of
      Just xs2@[_] -> [("-relmap", xs2)] ++ xs
      _ -> xs

likeRename :: C.RopParser'
likeRename xs =
    case lookup "" xs of
      Just xs2 -> [("-term", xs2)] ++ xs
      _ -> xs

likeSource :: C.RopParser'
likeSource xs =
    case lookup "" xs of
      Just (s:ns) -> [("-sign", [s]), ("-term", ns)] ++ xs
      _ -> xs

