{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Order
( copOrder
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)

import Koshucode.Baala.Vanilla.Value.Val




-- ----------------------  Order

copOrder :: [Named (ContentOp Val)]
copOrder =
 [ namedEager  "="    ordEq
 , namedEager  "<>"   ordNeq
 , namedEager  "<"    ordLt
 , namedEager  "<="   ordLte
 , namedEager  ">"    ordGt
 , namedEager  ">="   ordGte
 ]

ordBy :: (Val -> Val -> Bool) -> [Val] -> AbOr Val
ordBy p [x, y] = Right . boolValue   $ x `p` y
ordBy _ _      = Left  $ AbortLookup ""

ordEq   :: [Val] -> AbOr Val
ordEq   =  ordBy (==)

ordNeq  :: [Val] -> AbOr Val
ordNeq  =  ordBy (/=)

ordLt   :: [Val] -> AbOr Val
ordLt   =  ordBy (<)

ordLte  :: [Val] -> AbOr Val
ordLte  =  ordBy (<=)

ordGt   :: [Val] -> AbOr Val
ordGt   =  ordBy (>)

ordGte  :: [Val] -> AbOr Val
ordGte  =  ordBy (>=)

