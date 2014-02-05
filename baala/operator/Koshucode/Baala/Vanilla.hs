{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla
( vanillaGlobal,
  module Koshucode.Baala.Vanilla.Cop,
  module Koshucode.Baala.Vanilla.Quoter,
  module Koshucode.Baala.Vanilla.Rop,
  module Koshucode.Baala.Vanilla.Type,
) where

import Koshucode.Baala.Vanilla.Cop
import Koshucode.Baala.Vanilla.Quoter
import Koshucode.Baala.Vanilla.Rop
import Koshucode.Baala.Vanilla.Type

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as R
import qualified Koshucode.Baala.Minimal as R

vanillaGlobal :: C.Global VContent
vanillaGlobal =
    C.global { C.globalCops = vanillaCops
             , C.globalRops = vanillaRops ++
                              R.minimalRops ++
                              R.builtinRops }

