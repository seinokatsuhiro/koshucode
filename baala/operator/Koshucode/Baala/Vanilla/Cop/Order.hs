{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Order
( copsOrder
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Vanilla.Type as V

-- ----------------------
{- $Operators

 [@=@]     Equal.

 [@\<\>@]  Not equal.

 [@<@]     Less than.

 [@<=@]    Less than or equal.

 [@>@]     Grater than.

 [@>=@]    Grater than or equal.

-}

copsOrder :: [C.Cop V.VContent]
copsOrder =
    [ C.CopFun  "="    copEq
    , C.CopFun  "<>"   copNeq
    , C.CopFun  "<"    copLt
    , C.CopFun  "<="   copLte
    , C.CopFun  ">"    copGt
    , C.CopFun  ">="   copGte
    ]

copBy :: (V.VContent -> V.VContent -> Bool) -> V.VCop
copBy p [x, y] = Right . C.putBool $ x `p` y
copBy _ _      = Left  $ B.abortNotFound ""

copEq   :: V.VCop
copEq   =  copBy (==)

copNeq  :: V.VCop
copNeq  =  copBy (/=)

copLt   :: V.VCop
copLt   =  copBy (<)

copLte  :: V.VCop
copLte  =  copBy (<=)

copGt   :: V.VCop
copGt   =  copBy (>)

copGte  :: V.VCop
copGte  =  copBy (>=)

