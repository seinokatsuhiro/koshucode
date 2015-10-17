{-# OPTIONS_GHC -Wall #-}

-- | Rops and cops.

module Koshucode.Baala.Op.Global
  ( vanillaGlobal,
    vanillaRops,
    vanillaCops,
  ) where

import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Data      as D
import qualified Koshucode.Baala.Core      as C
import qualified Koshucode.Baala.Rop       as Rop
import qualified Koshucode.Baala.Rop.Nest  as Rop
import qualified Koshucode.Baala.Rop.Cox   as Rop
import qualified Koshucode.Baala.Cop       as Cop

-- | Global with operators.
vanillaGlobal :: (D.CContent c) => C.Global c
vanillaGlobal = global where
    global    = C.global { C.globalOpset     = C.opsetFill opset }
    opset     = C.opset  { C.opsetRopList    = vanillaRops
                         , C.opsetCop        = copset }
    copset    = D.copset { D.copsetCopList   = vanillaCops
                         , D.copsetInfixList = vanillaInfix }

-- | Relmap operators
vanillaRops :: (D.CContent c) => [C.Rop c]
vanillaRops = ropsCox ++ ropsNonCox

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

-- | Term-content operators.
vanillaCops :: (D.CContent c) => [D.Cop c]
vanillaCops = concat [ Cop.copsArith
                     , Cop.copsLogic
                     , Cop.copsList
                     , Cop.copsMisc
                     , Cop.copsOrder
                     , Cop.copsTime
                     , Cop.copsType ]

-- | Height table.
vanillaInfix :: [B.Named B.InfixHeight]
vanillaInfix = htab where
    h ! name = (name, Right h)

    htab =
        [ 9 ! "then"
        , 9 ! "when"
        , 9 ! "unless"
        , 9 ! "is"
        , 9 ! "of"
        , 9 ! "to"

        , 8 ! "or"

        , 7 ! "and"

        , 6 ! "in"
        , 6 ! "="     -- equal
        , 6 ! "<>"    -- not equal
        , 6 ! "<"     -- less than
        , 6 ! ">"     -- greater than
        , 6 ! "<="    -- less than or equal
        , 6 ! ">="    -- greater than or equal
        , 6 ! "=*"    -- begin with
        , 6 ! "*="    -- end with
        , 6 ! "*=*"   -- contain

        , 2 ! "+"
        , 2 ! "-"
        , 2 ! "++"
        , 2 ! "intersect"
        , 2 ! "minus"

        , 1 ! "*"
        , 1 ! "/"
        , 1 ! "quo"
        , 1 ! "rem"

        , ("<left-1>"  , Left 1)
        , ("<right-1>" , Right 1)
        ]

