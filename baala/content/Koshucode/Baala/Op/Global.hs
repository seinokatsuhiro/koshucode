{-# OPTIONS_GHC -Wall #-}

-- | Rops and cops.

module Koshucode.Baala.Op.Global
  ( baalaGlobal,
    baalaRops,
  ) where

import qualified Koshucode.Baala.Data      as D
import qualified Koshucode.Baala.Core      as C
import qualified Koshucode.Baala.Rop       as Rop
import qualified Koshucode.Baala.Rop.Nest  as Rop
import qualified Koshucode.Baala.Rop.Cox   as Rop
import qualified Koshucode.Baala.Cop       as Cop

-- | Global with operators.
baalaGlobal :: (D.CContent c) => C.Global c
baalaGlobal = global where
    global    = C.global { C.globalOpset     = C.opsetFill opset }
    opset     = C.opset  { C.opsetRopList    = baalaRops
                         , C.opsetCop        = copset }
    copset    = D.copset { D.copsetCopList   = Cop.baalaCops
                         , D.copsetInfixList = Cop.baalaInfix }

-- | Relmap operators
baalaRops :: (D.CContent c) => [C.Rop c]
baalaRops = ropsCox ++ ropsNonCox

ropsCox :: (D.CContent c) => [C.Rop c]
ropsCox    = Rop.ropsCoxAccessor
          ++ Rop.ropsCoxCalc
          ++ Rop.ropsCoxEmpty
          ++ Rop.ropsCoxFilter
          ++ Rop.ropsCoxGadget
          ++ Rop.ropsCoxRange

ropsNonCox :: (D.CContent c) => [C.Rop c]
ropsNonCox = Rop.ropsMeta
          ++ Rop.ropsResource
          ++ Rop.ropsNest
          ++ Rop.ropsCheck
          ++ Rop.ropsPeripheral
          ++ Rop.ropsControl
          ++ Rop.ropsGadget
          ++ Rop.ropsTerm
          ++ Rop.ropsTermGadget
          ++ Rop.ropsLattice
          ++ Rop.ropsSource
          ++ Rop.ropsBuiltin

