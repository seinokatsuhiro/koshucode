{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quasiquoter of Koshucode

module Koshucode.Baala.Op.Quoter
  ( koshu
  ) where

import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Type.Vanilla as Type

-- | Quasiquoter for @[koshu| ... |]@.
koshu :: C.QuasiQuoter
koshu = C.koshuQuoter vanillaLex [| vanillaFull |]

-- relmap constructors
vanillaLex  :: C.ConsLexmap
vanillaFull :: C.ConsRelmap Type.VContent
(C.RelmapCons vanillaLex vanillaFull)
    = C.relmapCons C.global

