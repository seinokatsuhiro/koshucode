{-# OPTIONS_GHC -Wall #-}

-- | Operand patterns

module Koshucode.Baala.Minimal.Relmap.Operand
( -- * Operand patterns
  MinimalOperand (..),

  -- * Operand parsers
  likePick,
  likeMeet,
  likeRename,
  likeSource,
) where

import qualified Koshucode.Baala.Builtin as Kit



-- ----------------------  Opernd paterns

-- | 'OpPattern' for minimal operators
data MinimalOperand
    = LikeId      -- ^ no operand
    | LikeMeet    -- ^ { @-relmap@ } relmap [ @-share@ \/name ... ]
    | LikePick    -- ^ { @-term@ } \/name ...
    | LikeRename  -- ^ { @-term@ } \/new \/old ...
    | LikeSource  -- ^ { @-sign@ } relsign { @-term@ } \/name ...
      deriving (Show, Eq, Enum)

instance Kit.OpPattern MinimalOperand where
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

likePick :: Kit.OpParser'
likePick xs =
    case lookup "" xs of
      Just xs2 -> [("-term", xs2)] ++ xs
      _ -> xs

likeMeet :: Kit.OpParser'
likeMeet xs =
    case lookup "" xs of
      Just xs2@[_] -> [("-relmap", xs2)] ++ xs
      _ -> xs

likeRename :: Kit.OpParser'
likeRename xs =
    case lookup "" xs of
      Just xs2 -> [("-term", xs2)] ++ xs
      _ -> xs

likeSource :: Kit.OpParser'
likeSource xs =
    case lookup "" xs of
      Just (s:ns) -> [("-sign", [s]), ("-term", ns)] ++ xs
      _ -> xs

