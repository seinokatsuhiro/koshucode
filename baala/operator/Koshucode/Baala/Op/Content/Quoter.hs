{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Op.Content.Quoter
( koshu
) where

import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Op.Vanilla.Rops
import Koshucode.Baala.Op.Content.Type

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: C.QuasiQuoter
koshu = C.koshuQuoter vanillaLex [| vanillaFull |]

-- relmap constructors
vanillaLex  :: C.RelmapConsLex
vanillaFull :: C.RelmapConsFull VContent
(C.RelmapCons vanillaLex vanillaFull)
    = C.relmapCons $ C.global { C.globalRops = vanillaRops }

