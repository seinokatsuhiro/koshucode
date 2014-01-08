{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Order
( copsOrder
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla.Type

-- ----------------------
{- $Operators

 [@=@]     Equal.

 [@\<\>@]  Not equal.

 [@<@]     Less than.

 [@<=@]    Less than or equal.

 [@>@]     Grater than.

 [@>=@]    Grater than or equal.

-}

copsOrder :: [B.Named (C.Cop VContent)]
copsOrder =
    [ C.copFun  "="    copEq
    , C.copFun  "<>"   copNeq
    , C.copFun  "<"    copLt
    , C.copFun  "<="   copLte
    , C.copFun  ">"    copGt
    , C.copFun  ">="   copGte
    ]

copBy :: (VContent -> VContent -> Bool) -> VCop
copBy p [x, y] = Right . C.putBool $ x `p` y
copBy _ _      = Left  $ B.abortNotFound ""

copEq   :: VCop
copEq   =  copBy (==)

copNeq  :: VCop
copNeq  =  copBy (/=)

copLt   :: VCop
copLt   =  copBy (<)

copLte  :: VCop
copLte  =  copBy (<=)

copGt   :: VCop
copGt   =  copBy (>)

copGte  :: VCop
copGte  =  copBy (>=)

