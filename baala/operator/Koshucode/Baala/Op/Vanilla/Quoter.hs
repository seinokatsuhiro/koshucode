{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Op.Vanilla.Quoter
( koshu
) where
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Op.Vanilla.Rop
import Koshucode.Baala.Op.Vanilla.Type

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: C.QuasiQuoter
koshu = C.koshuQuoter vanillaLex [| vanillaFull |]

-- relmap constructors
vanillaLex  :: C.RelmapConsLex
vanillaFull :: C.RelmapConsFull VContent
(C.RelmapCons vanillaLex vanillaFull)
    = C.relmapCons $ C.global { C.globalRops = vanillaRops }

