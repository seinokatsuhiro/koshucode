{-# OPTIONS_GHC -Wall #-}

-- | Basic relmap operators.

module Koshucode.Baala.Rop.Flat.Rops
  ( ropsFlat,
  ) where

import qualified Koshucode.Baala.DataPlus             as K
import qualified Koshucode.Baala.Core                 as C
import qualified Koshucode.Baala.Rop.Base             as Rop
import qualified Koshucode.Baala.Rop.Flat.Applied     as Rop
import qualified Koshucode.Baala.Rop.Flat.Check       as Rop
import qualified Koshucode.Baala.Rop.Flat.Control     as Rop
import qualified Koshucode.Baala.Rop.Flat.Elem        as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice     as Rop
import qualified Koshucode.Baala.Rop.Flat.Meta        as Rop
import qualified Koshucode.Baala.Rop.Flat.Order       as Rop
import qualified Koshucode.Baala.Rop.Flat.Resource    as Rop
import qualified Koshucode.Baala.Rop.Flat.Source      as Rop
import qualified Koshucode.Baala.Rop.Flat.Term        as Rop
import qualified Koshucode.Baala.Rop.Flat.TermGadget  as Rop

-- | Basic relmap operators.
ropsFlat :: (K.CContent c) => [C.Rop c]
ropsFlat = Rop.ropsMeta
        ++ Rop.ropsResource
        ++ Rop.ropsCheck
        ++ Rop.ropsElem
        ++ Rop.ropsControl
        ++ Rop.ropsApplied
        ++ Rop.ropsOrder
        ++ Rop.ropsTerm
        ++ Rop.ropsTermGadget
        ++ Rop.ropsLattice
        ++ Rop.ropsSource
        ++ Rop.ropsBuiltin

