{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Rop.Flat.Message
  ( module Koshucode.Baala.Rop.Base.Message,
    checkTerm,
    reqCollection,
    unexpTermName,
  ) where

import qualified Koshucode.Baala.DataPlus       as K
import qualified Koshucode.Baala.Core           as C
import Koshucode.Baala.Rop.Base.Message


-- ----------------------  Op package

-- | check-term failed
checkTerm :: String -> [K.TermName] -> K.Head -> K.Ab a
checkTerm label ns he =
    K.leftLines "check-term failed"
         $ detailTermRel label ns he

-- | Require collection type
reqCollection :: K.Ab a
reqCollection = K.leftBecause "Require collection type"

-- | Unexpected term names
unexpTermName :: K.Ab a
unexpTermName = K.leftBecause "Unexpected term names"

