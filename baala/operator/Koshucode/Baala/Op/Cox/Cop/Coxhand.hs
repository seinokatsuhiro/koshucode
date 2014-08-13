{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Cox.Cop.Coxhand
( -- * Application
  a, ax,
  -- * Function
  f, f1, f2, f3,
  -- * Variable
  v, v1, v2, v3,
) where

import qualified Koshucode.Baala.Base   as B
import qualified Koshucode.Baala.Core   as C


-- --------------------------------------------  Application

a :: String -> [C.Cox c] -> C.Cox c
a = ax . v

ax :: C.Cox c -> [C.Cox c] -> C.Cox c
ax = C.CoxApplyL []


-- --------------------------------------------  Function

f :: [String] -> B.Map (C.Cox c)
f vs = C.coxInsert . C.CoxDerivL [] Nothing vs

f1, f2, f3 :: B.Map (C.Cox c)
f1 = f ["#1"]
f2 = f ["#1", "#2"]
f3 = f ["#1", "#2", "#3"]


-- --------------------------------------------  Variable

v :: String -> C.Cox c
v n = C.CoxVar [] n 0

v1, v2, v3 :: C.Cox c
v1 = v "#1"
v2 = v "#2"
v3 = v "#3"

