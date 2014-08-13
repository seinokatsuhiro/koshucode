{-# OPTIONS_GHC -Wall #-}

-- | Term-content operators.

module Koshucode.Baala.Op.Cop.Cops
( vanillaGlobal,
  vanillaRops,
  vanillaCops,
) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Core              as C

import qualified Koshucode.Baala.Op.Builtin        as Rops
import qualified Koshucode.Baala.Op.Check          as Rops
import qualified Koshucode.Baala.Op.Control        as Rops
import qualified Koshucode.Baala.Op.Gadget         as Rops
import qualified Koshucode.Baala.Op.Lattice        as Rops
import qualified Koshucode.Baala.Op.Meta           as Rops
import qualified Koshucode.Baala.Op.Nest           as Rops
import qualified Koshucode.Baala.Op.Peripheral     as Rops
import qualified Koshucode.Baala.Op.Source         as Rops
import qualified Koshucode.Baala.Op.Term           as Rops

import qualified Koshucode.Baala.Op.Cox.Calc       as Rops
import qualified Koshucode.Baala.Op.Cox.Empty      as Rops
import qualified Koshucode.Baala.Op.Cox.Filter     as Rops

import qualified Koshucode.Baala.Op.Cop.Arith      as Op
import qualified Koshucode.Baala.Op.Cop.List       as Op
import qualified Koshucode.Baala.Op.Cop.Logic      as Op
import qualified Koshucode.Baala.Op.Cop.Order      as Op

vanillaGlobal :: (C.CContent c) => C.Global c
vanillaGlobal =
    C.global { C.globalCops = vanillaCops
             , C.globalRops = vanillaRops }

vanillaRops :: (C.CContent c) => [C.Rop c]
vanillaRops = ropsCox
           ++ Rops.ropsMeta
           ++ Rops.ropsNest
           ++ Rops.ropsCheck
           ++ Rops.ropsPeripheral
           ++ Rops.ropsControl
           ++ Rops.ropsGadget
           ++ Rops.ropsTerm
           ++ Rops.ropsLattice
           ++ Rops.ropsSource
           ++ Rops.ropsBuiltin

ropsCox :: (C.CContent c) => [C.Rop c]
ropsCox = Rops.ropsCoxCalc
       ++ Rops.ropsCoxEmpty
       ++ Rops.ropsCoxFilter

-- | Term-content operators and its height table.
vanillaCops :: (C.CContent c) => ([C.Cop c], [B.Named B.InfixHeight])
vanillaCops = (concat cops, htab) where

    cops = [ Op.copsArith
           , Op.copsLogic
           , Op.copsList
           , Op.copsOrder ]

    h ! name = (name, Right h)

    htab =
        [ 9 ! "then"
        , 9 ! "when"
        , 9 ! "unless"
        , 9 ! "is"

        , 8 ! "or"

        , 7 ! "and"

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
    
