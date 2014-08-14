{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Order
( copsOrder
  -- $Operators
) where

import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Op.Cop.Coxhand     as H
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
    [ C.CopFun  (C.copInfix "=")   copEq
    , C.CopFun  (C.copInfix "<>")  copNeq
    , C.CopFun  (C.copInfix "<")   copLt
    , C.CopFun  (C.copInfix "<=")  copLte
    , C.CopFun  (C.copInfix ">")   copGt
    , C.CopFun  (C.copInfix ">=")  copGte

    , orderPrefix    "="
    , orderPrefix    "<>"
    , orderPrefix    "<"
    , orderPrefix    "<="
    , orderPrefix    ">"
    , orderPrefix    ">="

    , orderPostfix   "="
    , orderPostfix   "<>"
    , orderPostfix   "<"
    , orderPostfix   "<="
    , orderPostfix   ">"
    , orderPostfix   ">="

    , C.CopCox  (C.copInfix "is")  copIs
    , C.CopCox  "between" copBetween
    , C.CopCox  "all"     $ copCollect "and"
    , C.CopCox  "any"     $ copCollect "or"
    ]

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
copIs _      = Message.unmatchType ""

orderPrefix :: String -> C.Cop c
orderPrefix op = C.CopCox (C.copPrefix op) $ cop where
    cop [x] = Right $ H.f1 $ H.ai op [H.v1, x]
    cop _   = Message.adlib "require operand"

orderPostfix :: String -> C.Cop c
orderPostfix op = C.CopCox (C.copPostfix op) $ cop where
    cop [x] = Right $ H.f1 $ H.ai op [x, H.v1]
    cop _   = Message.adlib "require operand"

copBetween :: C.CopCox c
copBetween [low, high] = Right between where
    between = H.f1 $ H.ai "and" [asc [low, H.v1], asc [H.v1, high]]
    asc     = H.ai "<="
copBetween _ = Message.adlib "require operand"

copCollect :: String -> C.CopCox c
copCollect op fs = Right $ H.f1 $ H.ai op (map ap fs) where
    ap f = H.ax f [H.v1]

