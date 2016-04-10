{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.Bundle
  ( ropsFlat,
  ) where

import qualified Koshucode.Baala.Data                 as D
import qualified Koshucode.Baala.Core                 as C
import qualified Koshucode.Baala.Rop.Base             as Rop
import qualified Koshucode.Baala.Rop.Flat.Check       as Rop
import qualified Koshucode.Baala.Rop.Flat.Control     as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice     as Rop
import qualified Koshucode.Baala.Rop.Flat.Gadget      as Rop
import qualified Koshucode.Baala.Rop.Flat.Meta        as Rop
import qualified Koshucode.Baala.Rop.Flat.Order       as Rop
import qualified Koshucode.Baala.Rop.Flat.Peripheral  as Rop
import qualified Koshucode.Baala.Rop.Flat.Resource    as Rop
import qualified Koshucode.Baala.Rop.Flat.Source      as Rop
import qualified Koshucode.Baala.Rop.Flat.Term        as Rop
import qualified Koshucode.Baala.Rop.Flat.TermGadget  as Rop

ropsFlat :: (D.CContent c) => [C.Rop c]
ropsFlat = Rop.ropsMeta
        ++ Rop.ropsResource
        ++ Rop.ropsCheck
        ++ Rop.ropsPeripheral
        ++ Rop.ropsControl
        ++ Rop.ropsGadget
        ++ Rop.ropsOrder
        ++ Rop.ropsTerm
        ++ Rop.ropsTermGadget
        ++ Rop.ropsLattice
        ++ Rop.ropsSource
        ++ Rop.ropsBuiltin

