{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Order
( copOrder
-- $Operators
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Core.Content

import Koshucode.Baala.Vanilla.Value.Content

-- ----------------------
{- $Operators

 [@=@]     Equal.

 [@\<\>@]  Not equal.

 [@<@]     Less than.

 [@<=@]    Less than or equal.

 [@>@]     Grater than.

 [@>=@]    Grater than or equal.

-}

copOrder :: [Named (Cop VContent)]
copOrder =
 [ namedEager  "="    ordEq
 , namedEager  "<>"   ordNeq
 , namedEager  "<"    ordLt
 , namedEager  "<="   ordLte
 , namedEager  ">"    ordGt
 , namedEager  ">="   ordGte
 ]

ordBy :: (VContent -> VContent -> Bool) -> [VContent] -> AbOr VContent
ordBy p [x, y] = Right . putBool $ x `p` y
ordBy _ _      = Left  $ AbortLookup ""

ordEq   :: [VContent] -> AbOr VContent
ordEq   =  ordBy (==)

ordNeq  :: [VContent] -> AbOr VContent
ordNeq  =  ordBy (/=)

ordLt   :: [VContent] -> AbOr VContent
ordLt   =  ordBy (<)

ordLte  :: [VContent] -> AbOr VContent
ordLte  =  ordBy (<=)

ordGt   :: [VContent] -> AbOr VContent
ordGt   =  ordBy (>)

ordGte  :: [VContent] -> AbOr VContent
ordGte  =  ordBy (>=)



