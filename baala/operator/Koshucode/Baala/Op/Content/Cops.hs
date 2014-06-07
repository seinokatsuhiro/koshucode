{-# OPTIONS_GHC -Wall #-}

-- | Term-content operators.

module Koshucode.Baala.Op.Content.Cops
( vanillaGlobal,
  vanillaCops,
) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Core              as C

import qualified Koshucode.Baala.Op.Builtin        as OpBase
import qualified Koshucode.Baala.Op.Cox            as OpBase
import qualified Koshucode.Baala.Op.Minimal        as OpBase
import qualified Koshucode.Baala.Op.Meta           as OpBase
import qualified Koshucode.Baala.Op.Nest           as OpBase
import qualified Koshucode.Baala.Op.Peripheral     as OpBase
import qualified Koshucode.Baala.Op.Source         as OpBase
import qualified Koshucode.Baala.Op.Vanilla        as OpBase

import qualified Koshucode.Baala.Op.Content.Arith  as Op
import qualified Koshucode.Baala.Op.Content.List   as Op
import qualified Koshucode.Baala.Op.Content.Logic  as Op
import qualified Koshucode.Baala.Op.Content.Order  as Op

vanillaGlobal :: (C.CContent c) => C.Global c
vanillaGlobal =
    C.global { C.globalCops = vanillaCops
             , C.globalRops = OpBase.metaRops ++
                              OpBase.nestRops ++
                              OpBase.peripheralRops ++
                              OpBase.coxRops ++
                              OpBase.vanillaRops ++
                              OpBase.minimalRops ++
                              OpBase.sourceRops ++
                              OpBase.builtinRops }

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
    
