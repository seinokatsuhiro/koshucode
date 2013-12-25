{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Vanilla.Quoter
( koshu
) where
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla.Relmap
import Koshucode.Baala.Vanilla.Type

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: C.QuasiQuoter
koshu = C.koshuQuoter vanillaHalf [| vanillaFull |]

-- relmap constructors
vanillaHalf :: C.RelmapConsHalf
vanillaFull :: C.RelmapConsFull VContent
(C.RelmapCons vanillaHalf vanillaFull)
    = C.relmapCons vanillaRops

