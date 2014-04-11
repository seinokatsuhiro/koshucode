{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Message
( -- * Core package
  module Koshucode.Baala.Core.Message,

  -- * Op package
  checkTerm,
  diffHead,
  dupTerm,
  dumpRel,
  noOperand,
  notNestRel,
  oddOperand,
  reqBool,
  reqRel,
  reqCollection,
  reqNewTerm,
  unexpTermName,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Core.Message

-- | check-term failed
checkTerm :: String -> [B.TermName] -> B.Relhead -> B.Ab a
checkTerm label ns he =
    Left $ B.abortLines "check-term failed"
         $ detailTermRel label ns he

-- | Different headings
diffHead :: [B.Relhead] -> B.Ab a
diffHead = Left . B.abortLines "Different headings" . map showHead

-- | Dump relation
dumpRel :: (B.Pretty c, C.CRel c) => B.Rel c -> B.Ab a
dumpRel r = Left $ B.abortPage "Dump relation" $ C.relTableLines r

-- | Duplicate term name
dupTerm :: [B.TermName] -> B.Relhead -> B.Ab a
dupTerm ns he =
    Left $ B.abortLines "Duplicate term name"
         $ detailTermRel "Duplicated" ns he

-- | Odd operand
oddOperand :: B.Ab a
oddOperand = Left $ B.abortBecause "Odd operand"

-- | Operand not found
noOperand :: B.Ab a
noOperand = Left $ B.abortBecause "Operand not found"

-- | Not a nested relation
notNestRel :: [B.TermName] -> B.Relhead -> B.Ab a
notNestRel ns he =
    Left $ B.abortLines "Not a nested relation"
         $ detailTermRel "Given" ns he

-- | Require Boolean
reqBool :: B.Ab a
reqBool = Left $ B.abortBecause "Require Boolean"

-- | Require relation
reqRel :: B.Ab a
reqRel = Left $ B.abortBecause "Require relation"

-- | Require collection type
reqCollection :: B.Ab a
reqCollection = Left $ B.abortBecause "Require collection type"

-- | Require new term names
reqNewTerm :: [B.TermName] -> B.Relhead -> B.Ab a
reqNewTerm ns he =
    Left $ B.abortLines "Require new term names"
         $ detailTermRel "Known" ns he

-- | Unexpected term names
unexpTermName :: B.Ab a
unexpTermName = Left $ B.abortBecause "Unexpected term names"

showHead :: B.Relhead -> String
showHead = unwords . B.headNames
