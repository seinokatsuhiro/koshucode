{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Vanilla.Quoter
( koshu
) where
import Koshucode.Baala.Core
import Koshucode.Baala.Vanilla.Relmap
import Koshucode.Baala.Vanilla.Type

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: QuasiQuoter
koshu = koshuQuoter vanillaHalf [| vanillaFull |]

-- relmap constructors
vanillaHalf :: RelmapHalfCons
vanillaFull :: RelmapFullCons VContent
(RelmapCons vanillaHalf vanillaFull)
    = relmapCons vanillaRops

