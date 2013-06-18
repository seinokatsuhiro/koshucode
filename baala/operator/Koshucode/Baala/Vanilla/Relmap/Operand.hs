{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Operand
( VanillaOperand (..)
) where

import qualified Koshucode.Baala.Minimal as Mini

-- | 'Mini.OperandPattern' for relational operations.
data VanillaOperand
    {-| Boolean expression -}
    = LikeHold

    {-| List of new term and expression -}
    | LikeVal

    {-| Relmap and maybe shared terms -}
    | LikeMeet

    {-| Relsign and list of terms -}
    | LikeSource

      deriving (Show, Eq, Enum)

instance Mini.OperandPattern VanillaOperand where
    operandParser' LikeHold   = Mini.likePick
    operandParser' LikeVal    = Mini.likePick
    operandParser' LikeMeet   = Mini.likeMeet
    operandParser' LikeSource = Mini.likeMeet

