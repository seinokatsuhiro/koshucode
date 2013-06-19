{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Vanilla.Quoter
( koshu
) where
import Koshucode.Baala.Minimal.OpeKit as Kit
import Koshucode.Baala.Vanilla.Relmap.Implement
import Koshucode.Baala.Vanilla.Value.Relval

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: QuasiQuoter
koshu = Kit.koshuQuoter vanillaHalf [| vanillaFull |]

-- relmap constructors
vanillaHalf :: RelmapHalfCons
vanillaFull :: RelmapFullCons Val
(RelmapCons vanillaHalf vanillaFull)
    = Kit.relmapCons vanillaRelmaps

