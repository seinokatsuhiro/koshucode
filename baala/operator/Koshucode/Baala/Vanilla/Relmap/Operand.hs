{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Operand
( VanillaOperand (..),
) where

import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Builtin

{-| 'Mini.RopPattern' for relational operations. -}
data VanillaOperand
    = LikeGroup        -- ^ Trunk @-term -relmap@
    | LikeHold
    | LikeId
    | LikeMeet         -- ^ 'Mini.likeMeet' sorter
    | LikePos          -- ^ 'C.trunkEnum' sorter
    | LikePrefix       -- ^ Trunk @-prefix -term@
    | LikePrefixChange -- ^ Trunk @-new -old@
    | LikeSize         -- ^ Trunk @-term@
    | LikeSource
    | LikeUnprefix     -- ^ Trunk @-prefix@
    | LikeVal
      deriving (Show, Eq, Enum)

instance Builtin.RopPattern VanillaOperand where
    ropSorter  LikeGroup         =  C.trunkBinary "-term" "-relmap"
    ropSorter  LikeHold          =  C.trunkElems "-term"
    ropSorter  LikeId            =  C.trunkId
    ropSorter  LikeMeet          =  C.trunkUnary "-relmap"
    ropSorter  LikePos           =  C.trunkEnum
    ropSorter  LikePrefix        =  C.trunkUncons "-prefix" "-term"
    ropSorter  LikePrefixChange  =  C.trunkBinary "-new" "-old"
    ropSorter  LikeSize          =  C.trunkUnary "-term"
    ropSorter  LikeSource        =  C.trunkUnary "-relmap"
    ropSorter  LikeUnprefix      =  C.trunkUnary "-prefix"
    ropSorter  LikeVal           =  C.trunkElems "-term"

    ropPart    LikeGroup         =  ["-term", "-relmap"]
    ropPart    LikeHold          =  ["-exp"]
    ropPart    LikeId            =  []
    ropPart    LikeMeet          =  ["-relmap"]
    ropPart    LikePos           =  ["-1", "-2"]
    ropPart    LikePrefix        =  ["-prefix", "-term"]
    ropPart    LikePrefixChange  =  ["-new", "-old"]
    ropPart    LikeSize          =  ["-term"]
    ropPart    LikeSource        =  ["-sign", "-term"]
    ropPart    LikeUnprefix      =  ["-prefix"]
    ropPart    LikeVal           =  ["-exp"]

