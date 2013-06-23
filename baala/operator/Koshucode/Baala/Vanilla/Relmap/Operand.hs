{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Operand
( VanillaOperand (..)
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import qualified Koshucode.Baala.Minimal as Mini

-- | 'Mini.OpPattern' for relational operations.
data VanillaOperand
    {-| Boolean expression -}
    = LikeHold

    {-| List of new term and expression -}
    | LikeVal

    {-| Relmap and maybe shared terms -}
    | LikeMeet

    {-| Prefix.
        @-prefix@, @-term@ -}
    | LikePrefix

    {-| Relsign and list of terms -}
    | LikeSource

    {-| Prefix.
        @-prefix@ -}
    | LikeUnprefix

    {-| Prefix.
        @-new@, @-old@ -}
    | LikePrefixChange

    {-| Size.
        @-term@ -}
    | LikeSize

      deriving (Show, Eq, Enum)

instance Mini.OpPattern VanillaOperand where
    operandParser' LikeHold          = Mini.likePick
    operandParser' LikeVal           = Mini.likePick
    operandParser' LikeMeet          = Mini.likeMeet
    operandParser' LikePrefix        = likePrefix
    operandParser' LikePrefixChange  = likePrefixChange
    operandParser' LikeUnprefix      = likeUnprefix
    operandParser' LikeSource        = Mini.likeMeet
    operandParser' LikeSize          = likeSize

likePrefix :: OpParser'
likePrefix xs =
    case lookup "" xs of
      Just (p:ns) -> [("-prefix", [p]), ("-term", ns)] ++ xs
      _ -> xs

likeUnprefix :: OpParser'
likeUnprefix xs =
    case lookup "" xs of
      Just [p] -> [("-prefix", [p])] ++ xs
      _ -> xs

likePrefixChange :: OpParser'
likePrefixChange xs =
    case lookup "" xs of
      Just [x,y] -> [("-new", [x]), ("-old", [y])] ++ xs
      _ -> xs

likeSize :: OpParser'
likeSize xs =
    case lookup "" xs of
      Just [n] -> [("-term", [n])] ++ xs
      _ -> xs

