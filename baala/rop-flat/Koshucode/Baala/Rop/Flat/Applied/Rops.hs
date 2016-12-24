{-# OPTIONS_GHC -Wall #-}

-- | Applied operators.

module Koshucode.Baala.Rop.Flat.Applied.Rops
  ( ropsApplied,
  ) where

import qualified Koshucode.Baala.DataPlus                      as K
import qualified Koshucode.Baala.Core                          as C
import qualified Koshucode.Baala.Rop.Flat.Applied.Gadget       as Rop
import qualified Koshucode.Baala.Rop.Flat.Applied.Peripheral   as Rop
import qualified Koshucode.Baala.Rop.Flat.Applied.Subtext      as Rop


-- | Applied operators.
ropsApplied :: (K.CContent c) => [C.Rop c]
ropsApplied = Rop.ropsAppliedGadget
           ++ Rop.ropsAppliedPeripheral
           ++ Rop.ropsAppliedSubtext

