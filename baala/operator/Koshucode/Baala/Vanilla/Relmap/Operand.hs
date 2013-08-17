
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Operand
( VanillaOperand (..),
  likePrefix,
  likeUnprefix,
  likePrefixChange,
  likeSize,
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
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

instance Builtin.RopPattern VanillaOperand where
    ropSorter  LikeId            = id
    ropSorter  LikeHold          = Mini.likePick
    ropSorter  LikeVal           = Mini.likePick
    ropSorter  LikeMeet          = Mini.likeMeet
    ropSorter  LikePrefix        = likePrefix
    ropSorter  LikePrefixChange  = likePrefixChange
    ropSorter  LikeUnprefix      = likeUnprefix
    ropSorter  LikeSource        = Mini.likeMeet
    ropSorter  LikeSize          = likeSize

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

likePrefix  :: C.RopSorter
likePrefix  =  C.ropPartNameBy f where
    f (p:ns) = [("-prefix", [p]), ("-term", ns)]
    f [] = []

likeUnprefix  :: C.RopSorter
likeUnprefix  =  C.ropPartName "-prefix"

likePrefixChange  :: C.RopSorter
likePrefixChange  =  C.ropPartNameBy f where
    f [x,y] = [("-new", [x]), ("-old", [y])]
    f _ = []

likeSize :: C.RopSorter
likeSize =  C.ropPartName "-term"

