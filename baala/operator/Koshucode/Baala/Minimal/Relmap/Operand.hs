{-# OPTIONS_GHC -Wall #-}

-- | Operand patterns

module Koshucode.Baala.Minimal.Relmap.Operand
( -- * Operand patterns
  MinimalOperand (..)

  -- * Operand parsers
, likePick
, likeMeet
, likeRename
, likeSource
) where

import Koshucode.Baala.Minimal.OpKit as Kit



-- ----------------------  Opernd paterns

-- | 'OpPattern' for minimal operators
data MinimalOperand
    = LikeEmpty   -- ^ no operand
    | LikeMeet    -- ^ { @-relmap@ } relmap [ @-share@ \/name ... ]
    | LikePick    -- ^ { @-term@ } \/name ...
    | LikeRename  -- ^ { @-term@ } \/new \/old ...
    | LikeSource  -- ^ { @-sign@ } relsign { @-term@ } \/name ...
      deriving (Show, Eq, Enum)

instance OpPattern MinimalOperand where
    opParser'  LikeEmpty  = id
    opParser'  LikeMeet   = likeMeet
    opParser'  LikePick   = likePick
    opParser'  LikeRename = likeRename
    opParser'  LikeSource = likeSource

    opPart     LikeEmpty  = []
    opPart     LikeMeet   = ["-relmap", "-share"]
    opPart     LikePick   = ["-term"]
    opPart     LikeRename = ["-term"]
    opPart     LikeSource = ["-sign", "-term"]

    opUsage    LikeEmpty  = [""]
    opUsage    LikeMeet   = ["RELMAP [-share /NAME ...]"]
    opUsage    LikePick   = ["/NAME ..."]
    opUsage    LikeRename = ["/NEW /OLD ..."]
    opUsage    LikeSource = ["RELSIGN /NAME ..."]



-- ----------------------  Opernd parsers

likePick :: OpParser'
likePick xs =
    case lookup "" xs of
      Just xs2 -> [("-term", xs2)] ++ xs
      _ -> xs

likeMeet :: OpParser'
likeMeet xs =
    case lookup "" xs of
      Just xs2@[_] -> [("-relmap", xs2)] ++ xs
      _ -> xs

likeRename :: OpParser'
likeRename xs =
    case lookup "" xs of
      Just xs2 -> [("-term", xs2)] ++ xs
      _ -> xs

likeSource :: OpParser'
likeSource xs =
    case lookup "" xs of
      Just (s:ns) -> [("-sign", [s]), ("-term", ns)] ++ xs
      _ -> xs

