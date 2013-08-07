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
 [ C.namedEager  "="    copEq
 , C.namedEager  "<>"   copNeq
 , C.namedEager  "<"    copLt
 , C.namedEager  "<="   copLte
 , C.namedEager  ">"    copGt
 , C.namedEager  ">="   copGte
 ]

copBy :: (VContent -> VContent -> Bool) -> VCop
copBy p [x, y] = Right . C.putBool $ x `p` y
copBy _ _      = Left  $ B.AbortLookup ""

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



