{-# OPTIONS_GHC -Wall #-}

-- | Rops and cops.

module Koshucode.Baala.Op.Global
( vanillaGlobal,
  vanillaRops,
  vanillaCops,
) where

import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Core      as C
import qualified Koshucode.Baala.Op        as Rop
import qualified Koshucode.Baala.Op.Cox    as Rop
import qualified Koshucode.Baala.Op.Cop    as Cop

-- | Global with operators.
vanillaGlobal :: (C.CContent c) => C.Global c
vanillaGlobal =
    C.global { C.globalCops = vanillaCops
             , C.globalRops = vanillaRops }

-- | Relmap operators
vanillaRops :: (C.CContent c) => [C.Rop c]
vanillaRops = ropsCox ++ ropsNonCox

ropsCox :: (C.CContent c) => [C.Rop c]
ropsCox    = Rop.ropsCoxCalc
          ++ Rop.ropsCoxEmpty
          ++ Rop.ropsCoxFilter

ropsNonCox :: (C.CContent c) => [C.Rop c]
ropsNonCox = Rop.ropsMeta
          ++ Rop.ropsNest
          ++ Rop.ropsCheck
          ++ Rop.ropsPeripheral
          ++ Rop.ropsControl
          ++ Rop.ropsGadget
          ++ Rop.ropsTerm
          ++ Rop.ropsLattice
          ++ Rop.ropsSource
          ++ Rop.ropsBuiltin

-- | Term-content operators and its height table.
vanillaCops :: (C.CContent c) => ([C.Cop c], [B.Named B.InfixHeight])
vanillaCops = (concat cops, htab) where

    cops = [ Cop.copsArith
           , Cop.copsLogic
           , Cop.copsList
           , Cop.copsOrder ]

    h ! name = (name, Right h)

    htab =
        [ 9 ! "then"
        , 9 ! "when"
        , 9 ! "unless"
        , 9 ! "is"

        , 8 ! "or"

        , 7 ! "and"

        , 6 ! "in"
        , 6 ! "="
        , 6 ! "<>"
        , 6 ! "<"
        , 6 ! ">"
        , 6 ! "<="
        , 6 ! ">="

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
    
