{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Order
  ( copsOrder
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Data               as C
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

    , C.CopCox  (C.copNormal "between") between
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

between :: C.CopCox c
between [low, high] = Right $ H.f1 $ (low `binAsc` H.b1) `binAnd` (H.b1 `binAsc` high)
between _ = Msg.adlib "require operand"

binAnd :: B.Bin (C.Cox c)
binAnd  = H.bin "and"

binAsc :: B.Bin (C.Cox c)
binAsc  = H.bin "<="

