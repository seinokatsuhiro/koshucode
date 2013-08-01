
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Operand
( VanillaOperand (..),
  likePrefix,
  likeUnprefix,
  likePrefixChange,
  likeSize,
) where

import Koshucode.Baala.Core
import Koshucode.Baala.Builtin hiding (LikeId)
import qualified Koshucode.Baala.Minimal as Mini

-- | 'Mini.OpPattern' for relational operations.
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

instance OpPattern VanillaOperand where
    opParser' LikeId            = id
    opParser' LikeHold          = Mini.likePick
    opParser' LikeVal           = Mini.likePick
    opParser' LikeMeet          = Mini.likeMeet
    opParser' LikePrefix        = likePrefix
    opParser' LikePrefixChange  = likePrefixChange
    opParser' LikeUnprefix      = likeUnprefix
    opParser' LikeSource        = Mini.likeMeet
    opParser' LikeSize          = likeSize

    opPart    LikeId            = ["-add", "-term"]
    opPart    LikeHold          = ["-exp"]
    opPart    LikeVal           = ["-exp"]
    opPart    LikeMeet          = ["-relmap"]
    opPart    LikePrefix        = ["-prefix", "-term"]
    opPart    LikePrefixChange  = ["-new", "-old"]
    opPart    LikeUnprefix      = ["-prefix"]
    opPart    LikeSource        = ["-sign", "-term"]
    opPart    LikeSize          = ["-term"]

    opUsage   _ = []

likePrefix :: RopParser'
likePrefix xs =
    case lookup "" xs of
      Just (p:ns) -> [("-prefix", [p]), ("-term", ns)] ++ xs
      _ -> xs

likeUnprefix :: RopParser'
likeUnprefix xs =
    case lookup "" xs of
      Just [p] -> [("-prefix", [p])] ++ xs
      _ -> xs

likePrefixChange :: RopParser'
likePrefixChange xs =
    case lookup "" xs of
      Just [x,y] -> [("-new", [x]), ("-old", [y])] ++ xs
      _ -> xs

likeSize :: RopParser'
likeSize xs =
    case lookup "" xs of
      Just [n] -> [("-term", [n])] ++ xs
      _ -> xs

