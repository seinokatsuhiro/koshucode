{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Op.Cox.Quoter
( koshu
) where

import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Cop     as Op
import qualified Koshucode.Baala.Op.Type    as Op

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: C.QuasiQuoter
koshu = C.koshuQuoter vanillaLex [| vanillaFull |]

-- relmap constructors
vanillaLex  :: C.ConsLexmap
vanillaFull :: C.ConsRelmap Op.VContent
(C.RelmapCons vanillaLex vanillaFull)
    = C.relmapCons $ C.global { C.globalRops = Op.vanillaRops }

