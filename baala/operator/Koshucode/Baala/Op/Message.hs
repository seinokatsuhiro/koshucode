{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Message
( -- * Core package
  module Koshucode.Baala.Core.Message,

  -- * Op package
  diffHead,
  noOperand,
  reqBool,
  reqCollection,
  reqNewTerm,
  unexpTermName,
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Message

-- | Different headings
diffHead :: [B.Relhead] -> B.Ab a
diffHead = Left . B.abortLines "Different headings" . map showHead

showHead :: B.Relhead -> String
showHead = unwords . B.headNames

-- | Operand not found
noOperand :: B.Ab a
noOperand = Left $ B.abortBecause "Operand not found"

-- | Require Boolean
reqBool :: B.Ab a
reqBool = Left $ B.abortBecause "Require Boolean"

-- | Require collection type
reqCollection :: B.Ab a
reqCollection = Left $ B.abortBecause "Require collection type"

-- | Require new term names
reqNewTerm :: [String] -> B.Ab a
reqNewTerm = Left . B.abortLines "Require new term names"

-- | Unexpected term names
unexpTermName :: B.Ab a
unexpTermName = Left $ B.abortBecause "Unexpected term names"

