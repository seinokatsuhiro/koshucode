{-# OPTIONS_GHC -Wall #-}

-- | Operators on content order.

module Koshucode.Baala.Cop.Order
  ( copsOrder
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Cop.Coxhand        as H
import qualified Koshucode.Baala.Cop.Message        as Msg


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

-- | Oerators on content order.
copsOrder :: (D.CBool c, Eq c, Ord c) => [D.Cop c]
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

    , D.CopCox  (D.copNormal "between") between
    ]

orderInfix :: (D.CBool c) => String -> (c -> c -> Bool) -> D.Cop c
orderInfix n f = D.CopCalc (D.copInfix n) g where
    g args = do (x, y) <- D.getRightArg2 args
                D.putBool $ x `f` y

orderPrefix :: String -> D.Cop c
orderPrefix n = D.CopCox (D.copPrefix n) $ cop where
    cop [x] = Right $ H.f1 (H.b1 `op` x)
    cop _   = Msg.adlib "require operand"
    op      = H.bin n

orderPostfix :: String -> D.Cop c
orderPostfix n = D.CopCox (D.copPostfix n) $ cop where
    cop [x] = Right $ H.f1 (x `op` H.b1)
    cop _   = Msg.adlib "require operand"
    op      = H.bin n

between :: D.CopCox c
between [low, high] = Right $ H.f1 $ (low `binAsc` H.b1) `binAnd` (H.b1 `binAsc` high)
between _ = Msg.adlib "require operand"

binAnd :: B.Bin (D.Cox c)
binAnd  = H.bin "and"

binAsc :: B.Bin (D.Cox c)
binAsc  = H.bin "<="

