{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Content.Order
( copsOrder
  -- $Operators
) where

import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Op.Content.Coxhand as H
import qualified Koshucode.Baala.Op.Message         as Message

-- ----------------------
-- $Operators
--
--  [@=@]     Equal.
--
--  [@\<\>@]  Not equal.
--
--  [@<@]     Less than.
--
--  [@<=@]    Less than or equal.
--
--  [@>@]     Grater than.
--
--  [@>=@]    Grater than or equal.
--

copsOrder :: (C.CBool c, Eq c, Ord c) => [C.Cop c]
copsOrder =
    [ orderCox  "="
    , orderCox  "<>"
    , orderCox  "<"
    , orderCox  "<="
    , orderCox  ">"
    , orderCox  ">="

    , C.CopFun  "&="   copEq
    , C.CopFun  "&<>"  copNeq
    , C.CopFun  "&<"   copLt
    , C.CopFun  "&<="  copLte
    , C.CopFun  "&>"   copGt
    , C.CopFun  "&>="  copGte

    , C.CopCox  "&is"  copIs
    , C.CopCox  "all"  $ copCollect "&and"
    , C.CopCox  "any"  $ copCollect "&or"
    ]

orderCox :: String -> C.Cop c
orderCox op = C.CopCox op $ cop where
    op'     = C.prefixName op
    cop [x] = Right $ H.f1 $ H.a op' [H.v1, x]
    cop _   = Message.adlib "require operand"

copBy :: (C.CBool c) => (c -> c -> Bool) -> C.CopFun c
copBy p [Right x, Right y] = C.putBool $ x `p` y
copBy _ _  = Message.notFound ""

copEq   :: (C.CBool c, Eq c) => C.CopFun c
copEq   =  copBy (==)

copNeq  :: (C.CBool c, Eq c) => C.CopFun c
copNeq  =  copBy (/=)

copLt   :: (C.CBool c, Ord c) => C.CopFun c
copLt   =  copBy (<)

copLte  :: (C.CBool c, Ord c) => C.CopFun c
copLte  =  copBy (<=)

copGt   :: (C.CBool c, Ord c) => C.CopFun c
copGt   =  copBy (>)

copGte  :: (C.CBool c, Ord c) => C.CopFun c
copGte  =  copBy (>=)

copIs :: C.CopCox c
copIs [x, f] = Right $ H.ax f [x]
copIs _ = Message.unmatchType ""

copCollect :: String -> C.CopCox c
copCollect op fs = Right $ H.f1 $ H.a op (map ap fs) where
    ap f = H.ax f [H.v1]

