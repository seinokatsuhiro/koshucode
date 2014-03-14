{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Op.Quoter
( koshu
) where

import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Op.Vanilla as Op
import qualified Koshucode.Baala.Op.Type as Op

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: C.QuasiQuoter
koshu = C.koshuQuoter vanillaLex [| vanillaFull |]

-- relmap constructors
vanillaLex  :: C.RelmapConsLex
vanillaFull :: C.RelmapConsFull Op.VContent
(C.RelmapCons vanillaLex vanillaFull)
    = C.relmapCons $ C.global { C.globalRops = Op.vanillaRops }

