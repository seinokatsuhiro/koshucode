{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Bundle
  ( ropsFlat,
  ) where

import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Rop.Builtin     as Rop
import qualified Koshucode.Baala.Rop.Check       as Rop
import qualified Koshucode.Baala.Rop.Control     as Rop
import qualified Koshucode.Baala.Rop.Lattice     as Rop
import qualified Koshucode.Baala.Rop.Gadget      as Rop
import qualified Koshucode.Baala.Rop.Meta        as Rop
import qualified Koshucode.Baala.Rop.Peripheral  as Rop
import qualified Koshucode.Baala.Rop.Resource    as Rop
import qualified Koshucode.Baala.Rop.Source      as Rop
import qualified Koshucode.Baala.Rop.Term        as Rop
import qualified Koshucode.Baala.Rop.TermGadget  as Rop

ropsFlat :: (D.CContent c) => [C.Rop c]
ropsFlat = Rop.ropsMeta
        ++ Rop.ropsResource
        ++ Rop.ropsCheck
        ++ Rop.ropsPeripheral
        ++ Rop.ropsControl
        ++ Rop.ropsGadget
        ++ Rop.ropsTerm
        ++ Rop.ropsTermGadget
        ++ Rop.ropsLattice
        ++ Rop.ropsSource
        ++ Rop.ropsBuiltin

