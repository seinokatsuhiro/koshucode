{-# OPTIONS_GHC -Wall #-}

-- | Operand patterns

module Koshucode.Baala.Minimal.Relmap.Operand
( MinimalOperand (..)
, likePick
, likeMeet
, likeRename
, likeSource
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Minimal.Relmap.Pattern

-- | 'OperandPattern' for minimal operators
data MinimalOperand
    {-| No operand -}
    = LikeEmpty

    {-| Relmap and maybe shared terms -}
    | LikeMeet

    {-| List of present terms -}
    | LikePick

    {-| List of new term and present term.
        @-term@ pairs of new and present terms -}
    | LikeRename

    {-| Relsign and list of terms.
        @-sign@ relsign
        @-term@ term names -}
    | LikeSource
      deriving (Show, Eq, Enum)

instance OperandPattern MinimalOperand where
    operandParser' LikeEmpty  = id
    operandParser' LikeMeet   = likeMeet
    operandParser' LikePick   = likePick
    operandParser' LikeRename = likeRename
    operandParser' LikeSource = likeSource

    operandUsage   LikeEmpty  = [""]
    operandUsage   LikeMeet   = ["RELMAP"]
    operandUsage   LikePick   = ["/NAME ..."]
    operandUsage   LikeRename = ["/NEW /OLD ..."]
    operandUsage   LikeSource = ["SIGN /NAME ..."]

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

