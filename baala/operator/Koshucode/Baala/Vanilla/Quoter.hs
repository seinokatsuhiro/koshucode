{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Vanilla.Quoter
( koshu
) where
import Koshucode.Baala.Core
import Koshucode.Baala.Vanilla.Relmap.Implement
import Koshucode.Baala.Vanilla.Type.Relval

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: QuasiQuoter
koshu = koshuQuoter vanillaHalf [| vanillaFull |]

-- relmap constructors
vanillaHalf :: RelmapHalfCons
vanillaFull :: RelmapFullCons VContent
(RelmapCons vanillaHalf vanillaFull)
    = relmapCons vanillaRops

