
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Operand
( VanillaOperand (..),
  likePrefix,
  likeUnprefix,
  likePrefixChange,
  likeSize,
) where

import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin hiding (LikeId)
import qualified Koshucode.Baala.Minimal as Mini

-- | 'Mini.RopPattern' for relational operations.
data VanillaOperand
    = LikeId

    {-| Boolean expression -}
    | LikeHold

    {-| List of new term and expression -}
    | LikeVal

    {-| Relmap and maybe shared terms -}
    | LikeMeet

    {-| Prefix.
        @-prefix@, @-term@ -}
    | LikePrefix

    {-| JudgePattern and list of terms -}
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

instance RopPattern VanillaOperand where
    ropParser' LikeId            = id
    ropParser' LikeHold          = Mini.likePick
    ropParser' LikeVal           = Mini.likePick
    ropParser' LikeMeet          = Mini.likeMeet
    ropParser' LikePrefix        = likePrefix
    ropParser' LikePrefixChange  = likePrefixChange
    ropParser' LikeUnprefix      = likeUnprefix
    ropParser' LikeSource        = Mini.likeMeet
    ropParser' LikeSize          = likeSize

    ropPart    LikeId            = ["-add", "-term"]
    ropPart    LikeHold          = ["-exp"]
    ropPart    LikeVal           = ["-exp"]
    ropPart    LikeMeet          = ["-relmap"]
    ropPart    LikePrefix        = ["-prefix", "-term"]
    ropPart    LikePrefixChange  = ["-new", "-old"]
    ropPart    LikeUnprefix      = ["-prefix"]
    ropPart    LikeSource        = ["-sign", "-term"]
    ropPart    LikeSize          = ["-term"]

    ropUsage   _ = []

likePrefix :: C.RopParser'
likePrefix xs =
    case lookup "" xs of
      Just (p:ns) -> [("-prefix", [p]), ("-term", ns)] ++ xs
      _ -> xs

likeUnprefix :: C.RopParser'
likeUnprefix xs =
    case lookup "" xs of
      Just [p] -> [("-prefix", [p])] ++ xs
      _ -> xs

likePrefixChange :: C.RopParser'
likePrefixChange xs =
    case lookup "" xs of
      Just [x,y] -> [("-new", [x]), ("-old", [y])] ++ xs
      _ -> xs

likeSize :: C.RopParser'
likeSize xs =
    case lookup "" xs of
      Just [n] -> [("-term", [n])] ++ xs
      _ -> xs

