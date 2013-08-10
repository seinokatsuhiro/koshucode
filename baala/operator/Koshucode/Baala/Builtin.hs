{-# OPTIONS_GHC -Wall #-}

{-| Kit for implementing relational operators -}

module Koshucode.Baala.Builtin
( 
  -- * Builtin
  module Data.Monoid,
  module Koshucode.Baala.Builtin.Get,
  module Koshucode.Baala.Builtin.Pattern,
  module Koshucode.Baala.Builtin.Term,

  BuiltinOperand (..),
  builtinRops,
) where

import Data.Monoid
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin.Get
import Koshucode.Baala.Builtin.Term
import Koshucode.Baala.Builtin.Pattern

{-| 'OpPattern' for builtin operators. -}
data BuiltinOperand
    = LikeId      -- ^ no operand
      deriving (Show, Eq, Enum)

instance OpPattern BuiltinOperand where
    opParser'  LikeId     = id

    opPart     LikeId     = []

    opUsage    LikeId     = [""]

builtinRops :: (Ord c) => [C.Rop c]
builtinRops = operators "builtin" [ ("|", LikeId, consConcat) ]

consConcat :: C.RopCons c
consConcat = Right . mconcat . C.ropSubmap

