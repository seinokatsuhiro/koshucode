{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla
( vanillaGlobal,
  module Koshucode.Baala.Op.Vanilla.Cop,
  module Koshucode.Baala.Op.Vanilla.Quoter,
  module Koshucode.Baala.Op.Vanilla.Rop,
  module Koshucode.Baala.Op.Vanilla.Type,
) where

import Koshucode.Baala.Op.Vanilla.Cop
import Koshucode.Baala.Op.Vanilla.Quoter
import Koshucode.Baala.Op.Vanilla.Rop
import Koshucode.Baala.Op.Vanilla.Type

import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Minimal as Op

vanillaGlobal :: C.Global VContent
vanillaGlobal =
    C.global { C.globalCops = vanillaCops
             , C.globalRops = vanillaRops ++
                              Op.minimalRops ++
                              Op.builtinRops }

