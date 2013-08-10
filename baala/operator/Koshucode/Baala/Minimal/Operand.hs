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

-- | 'OpPattern' for minimal operators
data MinimalOperand
    = LikeId      -- ^ no operand
    | LikeMeet    -- ^ { @-relmap@ } relmap [ @-share@ \/name ... ]
    | LikePick    -- ^ { @-term@ } \/name ...
    | LikeRename  -- ^ { @-term@ } \/new \/old ...
    | LikeSource  -- ^ { @-sign@ } relsign { @-term@ } \/name ...
      deriving (Show, Eq, Enum)

instance OpPattern MinimalOperand where
    opParser'  LikeId     = id
    opParser'  LikeMeet   = likeMeet
    opParser'  LikePick   = likePick
    opParser'  LikeRename = likeRename
    opParser'  LikeSource = likeSource

    opPart     LikeId     = []
    opPart     LikeMeet   = ["-relmap", "-share"]
    opPart     LikePick   = ["-term"]
    opPart     LikeRename = ["-term"]
    opPart     LikeSource = ["-sign", "-term"]

    opUsage    LikeId     = [""]
    opUsage    LikeMeet   = ["RELMAP [-share /NAME ...]"]
    opUsage    LikePick   = ["/NAME ..."]
    opUsage    LikeRename = ["/NEW /OLD ..."]
    opUsage    LikeSource = ["RELSIGN /NAME ..."]



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

