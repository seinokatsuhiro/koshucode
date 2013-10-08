{-# OPTIONS_GHC -Wall #-}

{-| Minimal operand patterns. -}

module Koshucode.Baala.Minimal.Operand
( MinimalOperand (..),
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin

{- | 'Builtin.RopPattern' for minimal operators -}
data MinimalOperand
    = LikeId
    | LikeMeet      -- ^ Trunk @-relmap@, Branch @-share@
    | LikePick      -- ^ Trunk @-term@
    | LikeSource    -- ^ Trunk @-sign -term@
      deriving (Show, Eq, Enum)

instance Builtin.RopPattern MinimalOperand where
    ropSorter   LikeId     = C.trunkId
    ropSorter   LikeMeet   = C.trunkUnary "-relmap"
    ropSorter   LikePick   = C.trunkElems "-term"
    ropSorter   LikeSource = C.trunkUncons "-sign" "-term"

    ropPart     LikeId     = []
    ropPart     LikeMeet   = ["-relmap", "-share"]
    ropPart     LikePick   = ["-term"]
    ropPart     LikeSource = ["-sign", "-term"]

