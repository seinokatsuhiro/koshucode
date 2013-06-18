{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Vanilla.Quoter
( koshu
) where
import Koshucode.Baala.Vanilla.Value.Relval
import Koshucode.Baala.Vanilla.Relmap.Implement
import qualified Koshucode.Baala.Base.Kit as Kit

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: Kit.QuasiQuoter
koshu = Kit.koshuQuoter vanillaHalf [| vanillaFull |]

-- relmap constructors
vanillaHalf :: Kit.RelmapHalfCons
vanillaFull :: Kit.RelmapFullCons Val
(Kit.RelmapCons vanillaHalf vanillaFull)
    = Kit.relmapCons vanillaRelmaps

