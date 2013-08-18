{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Operand
( -- * Operand pattern
  VanillaOperand (..),

  -- * Operand sorter
  likeGroup,
  likePrefix,
  likePrefixChange,
  likeSize,
  likeUnprefix,
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
import qualified Koshucode.Baala.Minimal as Mini


-- ---------------------- Operand patterns

{-| 'Mini.RopPattern' for relational operations. -}
data VanillaOperand
    = LikeGroup        -- ^ 'likeGroup' sorter
    | LikeHold
    | LikeId
    | LikeMeet         -- ^ 'Mini.likeMeet' sorter
    | LikePrefix       -- ^ 'likePrefix' sorter
    | LikePrefixChange -- ^ 'likePrefixChange' sorter
    | LikeSize         -- ^ 'likeSize' sorter
    | LikeSource
    | LikeUnprefix     -- ^ 'likeUnprefix' sorter
    | LikeVal
      deriving (Show, Eq, Enum)

instance Builtin.RopPattern VanillaOperand where
    ropSorter  LikeGroup         =  likeGroup
    ropSorter  LikeHold          =  Mini.likePick
    ropSorter  LikeId            =  id
    ropSorter  LikeMeet          =  Mini.likeMeet
    ropSorter  LikePrefix        =  likePrefix
    ropSorter  LikePrefixChange  =  likePrefixChange
    ropSorter  LikeSize          =  likeSize
    ropSorter  LikeSource        =  Mini.likeMeet
    ropSorter  LikeUnprefix      =  likeUnprefix
    ropSorter  LikeVal           =  Mini.likePick

    ropPart    LikeGroup         =  ["-term", "-relmap"]
    ropPart    LikeHold          =  ["-exp"]
    ropPart    LikeId            =  []
    ropPart    LikeMeet          =  ["-relmap"]
    ropPart    LikePrefix        =  ["-prefix", "-term"]
    ropPart    LikePrefixChange  =  ["-new", "-old"]
    ropPart    LikeSize          =  ["-term"]
    ropPart    LikeSource        =  ["-sign", "-term"]
    ropPart    LikeUnprefix      =  ["-prefix"]
    ropPart    LikeVal           =  ["-exp"]



-- ----------------------  Operand sorter

{-| This sorter recognizes @-term@ and @-relamp@ operands.

    > group /r a
    > group -term /r -relmap a -}
likeGroup          :: C.RopSorter
likeGroup          =  C.ropPartNameBy f where
    f [term, rel]  =  [("-term", [term]), ("-relmap", [rel])]
    f _            =  []

{-| This sorter recognizes @-prefix@ and @-term@ operands.

    > prefix /x- /a /b /c
    > prefix -prefix /x- -term /a /b /c -}
likePrefix         :: C.RopSorter
likePrefix         =  C.ropPartNameBy f where
    f (pre : term) =  [("-prefix", [pre]), ("-term", term)]
    f []           =  []

{-| This sorter recognizes @-new@ and @-old@ operands.

    > prefix-change /y- /x-
    > prefix-change -new /y- -old /x- -}
likePrefixChange   :: C.RopSorter
likePrefixChange   =  C.ropPartNameBy f where
    f [new, old]   =  [("-new", [new]), ("-old", [old])]
    f _            =  []

{-| This sorter recognizes @-term@ operand.

    > size /s
    > size -term /s -}
likeSize           :: C.RopSorter
likeSize           =  C.ropPartName "-term"

{-| This sorter recognizes @-prefix@ operand.

    > unprefix /x-
    > unprefix -prefix /x- -}
likeUnprefix       :: C.RopSorter
likeUnprefix       =  C.ropPartName "-prefix"

