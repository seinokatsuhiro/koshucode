{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Builtin.Operator
( BuiltinOperand (..),
  builtinRops,
  module Data.Monoid,

  -- * Operator
  -- $ListOfOperator
) where

import Data.Monoid
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin.Pattern

{-| 'RopPattern' for builtin operators. -}
data BuiltinOperand
    = LikeId      -- ^ no operand
      deriving (Show, Eq, Enum)

instance RopPattern BuiltinOperand where
    ropSorter   LikeId     = id
    ropPart     LikeId     = []

builtinRops :: [C.Rop c]
builtinRops = ropList "builtin" [ ("|", LikeId, ropConsConcat) ]

ropConsConcat :: C.RopCons c
ropConsConcat = Right . mconcat . C.ropSubmap

-- ----------------------
{- $ListOfOperator

   [/r/ @|@ /s/]   Append relmaps

-}

