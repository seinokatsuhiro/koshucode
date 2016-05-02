{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Nest.Message
  ( module Koshucode.Baala.Rop.Flat.Message,
    notNestRel,
  ) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Syntax  as D
import qualified Koshucode.Baala.Data    as D
import Koshucode.Baala.Rop.Flat.Message

-- | Not a nested relation
notNestRel :: [D.TermName] -> D.Head -> B.Ab a
notNestRel ns he =
    Left $ B.abortLines "Not a nested relation"
         $ detailTermRel "Given" ns he
