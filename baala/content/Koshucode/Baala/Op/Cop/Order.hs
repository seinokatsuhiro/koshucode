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
    [ orderInfix     "="   (==)
    , orderInfix     "<>"  (/=)
    , orderInfix     "<"   (<)
    , orderInfix     "<="  (<=)
    , orderInfix     ">"   (>)
    , orderInfix     ">="  (>=)

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

    , C.CopCox  (C.copInfix  "between") betweenInfix
    , C.CopCox  (C.copPrefix "between") betweenPrefix

    , C.CopCox  (C.copInfix "is")  copIs
    , C.CopCox  (C.copNormal "all")     $ copCollect "and"
    , C.CopCox  (C.copNormal "any")     $ copCollect "or"
    ]

orderInfix :: (C.CBool c) => String -> (c -> c -> Bool) -> C.Cop c
orderInfix n f = C.CopFun (C.copInfix n) g where
    g [Right x, Right y] = C.putBool $ x `f` y
    g _                  = Message.notFound ""

orderPrefix :: String -> C.Cop c
orderPrefix n = C.CopCox (C.copPrefix n) $ cop where
    cop [x] = Right $ H.f1 (H.b1 `op` x)
    cop _   = Message.adlib "require operand"
    op      = H.bin n

orderPostfix :: String -> C.Cop c
orderPostfix n = C.CopCox (C.copPostfix n) $ cop where
    cop [x] = Right $ H.f1 (x `op` H.b1)
    cop _   = Message.adlib "require operand"
    op      = H.bin n

betweenPrefix :: C.CopCox c
betweenPrefix [C.CoxRefill _ low [high]] = Right between where
    between = H.f1 $ (low `binAsc` H.b1) `binAnd` (H.b1 `binAsc` high)
betweenPrefix _ = Message.adlib "require operand"

betweenInfix :: C.CopCox c
betweenInfix [x, C.CoxRefill _ low [high]] = Right between where
    between = (low `binAsc` x) `binAnd` (x `binAsc` high)
betweenInfix _ = Message.adlib "require operand"

binAnd :: C.Cox c -> C.Cox c -> C.Cox c
binAnd  = H.bin "and"

binAsc :: C.Cox c -> C.Cox c -> C.Cox c
binAsc  = H.bin "<="

copIs :: C.CopCox c
copIs [x, f] = Right $ H.rx f [x]
copIs _      = Message.unmatchType ""

copCollect :: String -> C.CopCox c
copCollect n fs = Right $ H.f1 $ H.r (C.copInfix n) (map refill fs) where
    refill f = H.rx f [H.b1]

