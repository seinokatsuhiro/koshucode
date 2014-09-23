{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Order
( copsOrder
  -- $Operators
) where

import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Op.Cop.Coxhand     as H
import qualified Koshucode.Baala.Op.Message         as Msg

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

    , C.CopCox  (C.copNormal  "all")    $ copCollect "and"
    , C.CopCox  (C.copNormal  "any")    $ copCollect "or"

    , C.CopCox  (C.copInfix  "is")      copIs
    , C.CopCox  (C.copInfix  "of")      ofInfix
    , C.CopCox  (C.copInfix  "to")      toInfix
    ]

orderInfix :: (C.CBool c) => String -> (c -> c -> Bool) -> C.Cop c
orderInfix n f = C.CopCalc (C.copInfix n) g where
    g [Right x, Right y] = C.putBool $ x `f` y
    g _                  = Msg.notFound ""

orderPrefix :: String -> C.Cop c
orderPrefix n = C.CopCox (C.copPrefix n) $ cop where
    cop [x] = Right $ H.f1 (H.b1 `op` x)
    cop _   = Msg.adlib "require operand"
    op      = H.bin n

orderPostfix :: String -> C.Cop c
orderPostfix n = C.CopCox (C.copPostfix n) $ cop where
    cop [x] = Right $ H.f1 (x `op` H.b1)
    cop _   = Msg.adlib "require operand"
    op      = H.bin n

betweenPrefix :: C.CopCox c
betweenPrefix [C.CoxFill _ low [high]] = Right between where
    between = H.f1 $ (low `binAsc` H.b1) `binAnd` (H.b1 `binAsc` high)
betweenPrefix _ = Msg.adlib "require operand"

betweenInfix :: C.CopCox c
betweenInfix [x, C.CoxFill _ low [high]] = Right between where
    between = (low `binAsc` x) `binAnd` (x `binAsc` high)
betweenInfix _ = Msg.adlib "require operand"

binAnd :: C.Cox c -> C.Cox c -> C.Cox c
binAnd  = H.bin "and"

binAsc :: C.Cox c -> C.Cox c -> C.Cox c
binAsc  = H.bin "<="

copIs :: C.CopCox c
copIs [x, f] = Right $ H.ix f [x]
copIs _      = Msg.unmatchType ""

copCollect :: String -> C.CopCox c
copCollect n fs = Right $ H.f1 $ H.ib (C.copInfix n) (map fill fs) where
    fill f = H.ix f [H.b1]

ofInfix :: C.CopCox c
ofInfix [f, x] = Right $ H.ix f [x]
ofInfix _ = Msg.adlib "require operand"

toInfix :: C.CopCox c
toInfix [x, f] = Right $ H.ix f [x]
toInfix _ = Msg.adlib "require operand"

