{-# OPTIONS_GHC -Wall #-}

-- | Relational operators with content calculation.

module Koshucode.Baala.Rop.Cox.Rops
  ( ropsCox,
  ) where

import qualified Koshucode.Baala.DataPlus            as K
import qualified Koshucode.Baala.Core                as C
import qualified Koshucode.Baala.Rop.Cox.Calc        as Rop
import qualified Koshucode.Baala.Rop.Cox.Empty       as Rop
import qualified Koshucode.Baala.Rop.Cox.Filter      as Rop
import qualified Koshucode.Baala.Rop.Cox.Gadget      as Rop
import qualified Koshucode.Baala.Rop.Cox.Range       as Rop
import qualified Koshucode.Baala.Rop.Cox.Type.Clock  as Rop
import qualified Koshucode.Baala.Rop.Cox.Type.Dec    as Rop

-- | Relational operators with content calculation.
ropsCox :: (K.CContent c) => [C.Rop c]
ropsCox    = Rop.ropsCoxCalc
          ++ Rop.ropsCoxEmpty
          ++ Rop.ropsCoxFilter
          ++ Rop.ropsCoxGadget
          ++ Rop.ropsCoxRange
          ++ Rop.ropsTypeClock
          ++ Rop.ropsTypeDec

