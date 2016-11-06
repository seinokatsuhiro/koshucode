{-# OPTIONS_GHC -Wall #-}

-- | Relational operators with content calculation.

module Koshucode.Baala.Rop.Cox.Bundle
  ( ropsCox,
  ) where

import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Cox.Accessor  as Rop
import qualified Koshucode.Baala.Rop.Cox.Calc      as Rop
import qualified Koshucode.Baala.Rop.Cox.Empty     as Rop
import qualified Koshucode.Baala.Rop.Cox.Filter    as Rop
import qualified Koshucode.Baala.Rop.Cox.Gadget    as Rop
import qualified Koshucode.Baala.Rop.Cox.Range     as Rop

-- | Relational operators with content calculation.
ropsCox :: (D.CContent c) => [C.Rop c]
ropsCox    = Rop.ropsCoxAccessor
          ++ Rop.ropsCoxCalc
          ++ Rop.ropsCoxEmpty
          ++ Rop.ropsCoxFilter
          ++ Rop.ropsCoxGadget
          ++ Rop.ropsCoxRange

