{-# OPTIONS_GHC -Wall #-}

-- | Helper functions to construct content expressions.

module Koshucode.Baala.Op.Cop.Coxhand
  ( -- * Form
    f, f1, f2, f3,
    -- * Fill
    i, ib, ix, bin,
    -- * Blank
    b, b1, b2, b3,
  ) where

import qualified Koshucode.Baala.Base   as B
import qualified Koshucode.Baala.Data   as B
import qualified Koshucode.Baala.Data   as C


-- --------------------------------------------  Form

-- | Create a form with named blanks.
f :: [String] -> B.Map (C.Cox c)
f vs = C.coxForm [] Nothing vs

-- | Shorthand for one-blank form — @f [\"\#1\"]@
f1 :: B.Map (C.Cox c)
f1 = f ["#1"]

-- | Shorthand for two-blanks form — @f [\"\#1\", \"\#2\"]@
f2 :: B.Map (C.Cox c)
f2 = f ["#1", "#2"]

-- | Shorthand for three-blanks form — @f [\"\#1\", \"\#2\", \"\#3\"]@
f3 :: B.Map (C.Cox c)
f3 = f ["#1", "#2", "#3"]


-- --------------------------------------------  Fill

-- | Fill blanks in a named form.
i :: String -> [C.Cox c] -> C.Cox c
i = ix . b

ib :: B.BlankName -> [C.Cox c] -> C.Cox c
ib = ix . C.CoxBlank []

-- | Fill blanks in the given form.
ix :: C.Cox c -> [C.Cox c] -> C.Cox c
ix = C.CoxFill []

-- | Fill two blanks in a named binary form.
bin :: String -> C.Cox c -> C.Cox c -> C.Cox c
bin n x y = ib (C.copInfix n) [x, y]


-- --------------------------------------------  Blank

-- | Create a named blank in a form.
b :: String -> C.Cox c
b = C.CoxBlank [] . B.BlankNormal

-- | Shorthand for the first blank — @b \"\#1\"@
b1 :: C.Cox c
b1 = b "#1"

-- | Shorthand for the second blank — @b \"\#2\"@
b2 :: C.Cox c
b2 = b "#2"

-- | Shorthand for the third blank — @b \"\#3\"@
b3 :: C.Cox c
b3 = b "#3"

