{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Message
( -- * Core package
  module Koshucode.Baala.Core.Message,

  -- * Op package
  noOperand,
  reqBool,
  reqNewTerm,
  unexpTermName,
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Message

-- | Operand not found
noOperand :: B.Ab a
noOperand = Left $ B.abortBecause "Operand not found"

-- | Require Boolean
reqBool :: B.Ab a
reqBool = Left $ B.abortBecause "Require Boolean"

-- | Require new term names
reqNewTerm :: [String] -> B.Ab a
reqNewTerm = Left . B.abortLines "Require new term names"

-- | Unexpected term names
unexpTermName :: B.Ab a
unexpTermName = Left $ B.abortBecause "Unexpected term names"

